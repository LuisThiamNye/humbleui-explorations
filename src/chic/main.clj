(ns chic.main
  (:require
   [chic.debug :as debug]
   [chic.ui :as cui]
   [chic.ui.layout :as cuilay]
   [clojure.string :as str]
   [chic.filebwr :as filebwr]
   [chic.error :as error]
   [chic.cljbwr :as cljbwr]
   [chic.windows :as windows]
   [chic.focus :as focus]
   [nrepl.server :as nrepl-server]
   [chic.text-editor :as text-editor]
   [chic.text-editor.core :as text-editor.core]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.paint :as paint]
   [io.github.humbleui.profile :as profile]
   [nrepl.cmdline :as nrepl]
   [cider.nrepl :refer [cider-nrepl-handler]]
   [chic.style :as style]
   [io.github.humbleui.window :as huiwin]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.jwm App EventFrame EventMouseButton EventMouseMove EventMouseScroll
    EventKey Window EventWindowFocusOut]
   [io.github.humbleui.skija Canvas FontMgr FontStyle Typeface Font Paint PaintMode]
   [io.github.humbleui.types IPoint]))

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

(def *app-root (volatile! nil))

(defn build-app-root []
  (ui/dynamic
    ctx [scale (:scale ctx)
         window (:chic/current-window ctx)]
    (let [font-ui (Font. style/face-default (float (* 14 scale)))
          leading (-> font-ui .getMetrics .getCapHeight Math/ceil (/ scale))
          fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))]
      (ui/with-context {:face-ui style/face-default
                        :face-code style/face-code-default
                        :font-code (Font. style/face-code-default (float (* 14 scale)))
                        :font-ui font-ui
                        :focus-manager focus-manager
                        :leading leading
                        :fill-text fill-text}
        (cuilay/column
         [:stretch 1
          (cui/dyncomp
           (filebwr/basic-view))]
         #_[:stretch 1
            (ui/gap 0 0)]
         (cuilay/height
          20
          (ui/fill
           (doto (Paint.) (.setColor (unchecked-int 0x11000000)))
           (cuilay/row
            key-indicator
            [:stretch 1
             (ui/gap 0 0)]
            (ui/clickable
             #(windows/remount-window window)
             (ui/fill (doto (Paint.) (.setColor (unchecked-int 0x11000000)))
                      (ui/valign
                       0.5 (ui/padding 20 0 (ui/label "Reload" font-ui fill-text)))))))))))))



(defn make-main-window []
  (let [screen (last (hui/screens))
        scale (:scale screen)
        width (* 600 scale)
        height (* 400 scale)
        area (:work-area screen)
        x (:x area)
        y (-> (:height area) (- height) (/ 2) (+ (:y area)))]
    (doto
     (windows/make
      {:id "main"
       :*app-root *app-root
       :build-app-root #'build-app-root
       :on-close #(fn [])})
      (huiwin/set-title "Chic")
      (huiwin/set-window-size width height)
      (huiwin/set-window-position x y)
      (huiwin/set-visible true))))

(defn -main [& args]
  (future (apply nrepl/-main args))
  (future
    (Thread/sleep 1000)
    (spit ".nrepl-port"
          (:port (nrepl-server/start-server
                  :port 7888 :handler cider-nrepl-handler))))
  (hui/start #(make-main-window)))

(comment
  (require 'chic.main :reload-all)
  (do
    (hui/doui
     (some-> (some #(when (= "main" (:id %)) %) (vals @windows/*windows))
             :window-obj huiwin/close)
     (make-main-window)))

  (chic.windows/request-frame (first (vals @windows/*windows)))

  @(:*ctx (first (vals @windows/*windows)))
  (:*ui-error (second (vals @windows/*windows)))
  ( @*app-root)

  (remount-app)

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
