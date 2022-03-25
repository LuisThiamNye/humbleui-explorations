(ns chic.main
  (:require
   [chic.debug :as debug]
   [chic.focus :as focus]
   [nrepl.server :as nrepl-server]
   [chic.text-editor :as text-editor]
   [chic.text-editor.core :as text-editor.core]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.paint :as paint]
   [io.github.humbleui.profile :as profile]
   [nrepl.cmdline :as nrepl]
   [cider.nrepl :refer [cider-nrepl-handler]]
   [io.github.humbleui.window :as window]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.jwm App EventFrame EventMouseButton EventMouseMove EventMouseScroll
    EventKey Window EventWindowFocusOut]
   [io.github.humbleui.skija Canvas FontMgr FontStyle Typeface Font Paint PaintMode]
   [io.github.humbleui.types IPoint]))

(defonce *window (atom nil))

(def ^Typeface face-default
  (.matchFamiliesStyle (FontMgr/getDefault)
                       (into-array String ["Roboto Slab", ".SF NS", "Helvetica Neue", "Arial"])
                       FontStyle/NORMAL))

(def ^Typeface face-code-default
  (Typeface/makeFromFile "/Volumes/Carbonator/csync/fonts/Input-Font/Input_Fonts/InputSans/InputSansCondensed/InputSansCondensed-Regular.ttf"))

(def editor (text-editor.core/make
             {:pos 0
              :face-default face-default}))
(comment
  (:focus-node editor)
  (:state editor)
  ;;
  )

(def *pressed-keys (volatile! #{}))
(def focus-manager (focus/new-manager))

(def key-indicator
  (ui/dynamic
   ctx [pressed-keys @*pressed-keys
        font-ui (:font-ui ctx)
        fill-text (:fill-text ctx)]
   (let [pressed-keys pressed-keys
         font-ui font-ui
         fill-text fill-text]
     (ui/on-key-down
      #(vswap! *pressed-keys conj (:hui.event.key/key %))
      (ui/on-key-up
       #(vswap! *pressed-keys disj (:hui.event.key/key %))
       (let [s (apply str
                      (eduction
                       (remove nil?)
                       [(when (contains? pressed-keys "Ctrl")
                          "C-")
                        (when (or (contains? pressed-keys "Option")
                                  (contains? pressed-keys "Alt"))
                          "A-")
                        (when (contains? pressed-keys "Command")
                          "H-")
                        (when (contains? pressed-keys "Shift")
                          "S-")
                        (when-let [letters (seq (eduction (filter #(== 1 (count %))) pressed-keys))]
                          (apply str letters))]))]
         (ui/label s font-ui fill-text)))))))

(declare app remount-app)

(defn remount-app []
  (alter-var-root
   #'app
   (fn [_]
     (ui/dynamic
      ctx [scale (:scale ctx)]
      (let [font-ui (Font. face-default (float (* 13 scale)))
            leading (-> font-ui .getMetrics .getCapHeight Math/ceil (/ scale))
            fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))]
        (ui/with-context {:face-ui face-default
                          :face-code face-code-default
                          :font-code (Font. face-code-default (float (* 13 scale)))
                          :font-ui font-ui
                          :focus-manager focus-manager
                          :leading leading
                          :fill-text fill-text}
          (ui/column
           (ui/padding
            20 leading
            (text-editor/element editor))
           [:stretch 1
            (ui/gap 0 0)]
           (ui/height
            20
            (ui/fill
             (doto (Paint.) (.setColor (unchecked-int 0x11000000)))
             (ui/row
              key-indicator
              [:stretch 1
               (ui/gap 0 0)]
              (ui/clickable
               #(remount-app)
               (ui/fill (doto (Paint.) (.setColor (unchecked-int 0x11000000)))
                        (ui/valign
                         0.5 (ui/label "Reload" font-ui fill-text)))))))))))))
  (some-> @*window window/request-frame))

(remount-app)

(defn on-paint [window ^Canvas canvas]
  (.clear canvas (unchecked-int 0xFFF6F6F6))
  (let [bounds (window/content-rect window)
        ctx {:scale (window/scale window)}]
    (profile/reset)
    ; (profile/measure "frame"
    (ui/draw app ctx bounds canvas)
    (profile/log)
    #_(window/request-frame window)))

(defn on-event [window event]
  (try
    (let [changed? (condp instance? event
                     EventMouseMove
                     (let [pos (IPoint. (.getX ^EventMouseMove event) (.getY ^EventMouseMove event))
                           event {:hui/event :hui/mouse-move
                                  :hui.event/pos pos}]
                       (ui/event app event))

                     EventMouseButton
                     (let [event {:hui/event :hui/mouse-button
                                  :hui.event.mouse-button/is-pressed (.isPressed ^EventMouseButton event)}]
                       (ui/event app event))

                     EventMouseScroll
                     (ui/event app
                               {:hui/event :hui/mouse-scroll
                                :hui.event.mouse-scroll/dx (.getDeltaX ^EventMouseScroll event)
                                :hui.event.mouse-scroll/dy (.getDeltaY ^EventMouseScroll event)})

                     EventWindowFocusOut
                     (do (vreset! *pressed-keys #{})
                         (prn "Focus out")
                         false)

                     EventKey
                     (ui/event app
                               {:hui/event (if (.isPressed ^EventKey event) :hui/key-down :hui/key-up)
                                :hui.event.key/key (.getName (.getKey ^EventKey event))
                                :eventkey event})

                     nil)]
      (when changed?
        (window/request-frame window)))
    (catch Exception e
      (debug/println-main (pr-str e)))))

(defn make-window []
  (let [screen (last (hui/screens))
        scale (:scale screen)
        width (* 600 scale)
        height (* 400 scale)
        area (:work-area screen)
        x (:x area)
        y (-> (:height area) (- height) (/ 2) (+ (:y area)))]
    (doto
     (window/make
      {:on-close #(reset! *window nil)
       :on-paint #'on-paint
       :on-event #'on-event})
      (window/set-title "Chic")
      (window/set-window-size width height)
      (window/set-window-position x y)
      (window/set-visible true))))

(defn -main [& args]
  ;; (future (apply nrepl/-main args))
  (spit ".nrepl-port"
        (:port (nrepl-server/start-server
                :port 7888 :handler cider-nrepl-handler)))
  (hui/start #(reset! *window (make-window))))

(comment
  (do
    (hui/doui (some-> @*window window/close))
    (reset! *window (hui/doui (make-window))))

  (remount-app)
  @(:state editor)
  (-> @(:state editor)
      :lines-by-id vals vec (get 2) :ui-segments
      (->> (map (comp :x :size :ui))))
  ;; 16 8 116
  ;;  8 8 124
  (System/gc)
  ;; a single scope/chain for shortcuts/key events where there can only be one handler at most.
  ;; widgets register/deregister their shortcuts as they come into/out of focus
  ;;   or reg/dereg upon mount/unmount and pred includes whether it is focussed.

  (.write (get @(:session (val (first @cider.nrepl.middleware.out/tracked-sessions-map)))
               #'*out*)
          "xyz")
  (.println (System/out) "x")
  (.println *out* "x")

#!
  )
