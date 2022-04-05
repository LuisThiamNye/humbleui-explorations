(ns chic.main
  (:require
   [babashka.fs :as fs]
   [rebel-readline.core]
   [rebel-readline.clojure.line-reader]
   [rebel-readline.clojure.service.local]
   [rebel-readline.clojure.main]
   [clojure.main]
   [io.github.humbleui.paint :as huipaint]
   [io.github.humbleui.core :as hui]
   [borkdude.dynaload :refer [dynaload]]
   [io.github.humbleui.ui :as ui]
   [io.github.humbleui.window :as huiwin]
   [nrepl.server :as nrepl-server])
  (:import
   [io.github.humbleui.skija Font Paint]))

(require '[cheshire.core]
         '[clojure.tools.reader]
         '[taoensso.encore]
         '[clojure.core.async]
         '[clojure.tools.analyzer.jvm]
         '[clojure-lsp.api]
         '[tech.droit.fset])

(set! *warn-on-reflection* true)

(require '[chic.debug.swap :as debug.swap])
;; (debug.swap/install-all-instrumentation!)

(require '[chic.focus :as focus]
         '[chic.style :as style]
         '[chic.debug.debugger :as debugger]
         '[chic.debug :as debug]
         '[chic.ui.event :as uievt]
         '[chic.ui :as cui]
         '[chic.demo :as demo]
         '[chic.ui.layout :as cuilay]
         '[chic.windows :as windows])
(debug.swap/install-all-instrumentation!)

(def *cider-nrepl-handler (dynaload 'cider.nrepl/cider-nrepl-handler {:default nil}))

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
         fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))
         font-code (Font. style/face-code-default (float (* 14 scale)))]
     (ui/with-context {:face-ui style/face-default
                       :face-code style/face-code-default
                       :font-code font-code
                       :font-ui font-ui
                       :focus-manager focus-manager
                       :leading leading
                       :fill-text fill-text}
       (cuilay/column
        [:stretch 1
         (cui/dyncomp
          (demo/basic-view))]
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
           (cui/clickable
            (uievt/on-primary-down (fn [_](debugger/show-error-window)))
            (ui/dynamic
              _ctx [error-count (count (:error-stack @debugger/*state))]
              (ui/fill (huipaint/fill (if (pos? error-count)
                                        0x20503000
                                        0x09000000))
                       (cuilay/valign
                        0.5 (cuilay/padding 10 0 (ui/label (str error-count " E") font-ui fill-text))))))
           (ui/contextual
            (fn [{:keys [:chic.profiling/time-since-last-paint]}]
              (let [{:keys [latest-paint-duration]} @(:*profiling window)
                    millis (/ latest-paint-duration 1000000.)]
                (ui/fill
                 (huipaint/fill (if (< 16000000 latest-paint-duration)
                                  0xFFFF0000
                                  0x00FFFFFF))
                 (cuilay/width
                  150
                  (cuilay/halign
                   1 (cuilay/padding
                    5 0 (cuilay/valign
                         0.5 (ui/label (format "%3d fps %6.3f ms"
                                               (unchecked-int (/ 1000000000 time-since-last-paint))
                                               millis)
                                       font-code fill-text)))))))))
           (cui/clickable
            (fn [event]
              (when (:hui.event.mouse-button/is-pressed event)
                (windows/remount-window window)))
            (ui/fill (huipaint/fill 0x11000000)
                     (cuilay/valign
                      0.5 (cuilay/padding 20 0 (ui/label "Reload" font-ui fill-text)))))))))))))

(defn make-main-window []
  (let [screen (last (hui/screens))
        scale (:scale screen)
        width (* 600 scale)
        height (* 400 scale)
        area (:work-area screen)
        x (:x area)
        y (-> (:height area) (- height))]
    (doto
     (windows/make
      {:id "main"
       :*app-root *app-root
       :build-app-root #(cui/dyncomp (build-app-root))
       :on-close #(fn [])})
      (huiwin/set-title "Chic")
      (huiwin/set-window-size width height)
      (huiwin/set-window-position x y)
      (huiwin/set-visible true))))

(defn -main [& args]
  (.start
   (Thread.
    (fn []
      (rebel-readline.core/with-line-reader
        (rebel-readline.clojure.line-reader/create
         (rebel-readline.clojure.service.local/create))
        (clojure.main/repl
         :prompt (fn [])
         :read (rebel-readline.clojure.main/create-repl-read))))))
  (.start
   (Thread.
    (fn []
      (spit ".nrepl-port"
           (:port (nrepl-server/start-server
                   :port 7888
                   (if-some [cider-nrepl-handler @*cider-nrepl-handler]
                     {:handler cider-nrepl-handler}
                     {})))))))
  (.addShutdownHook
   (Runtime/getRuntime)
   (Thread. (fn [] (fs/delete-if-exists ".nrepl-port"))))
  (debugger/install-debug-ctx!)
  (debug/attach-vm!)
  (hui/start #(make-main-window)))

(comment
  (do
    (hui/doui
     (some-> (some #(when (= "main" (:id %)) %) (vals @windows/*windows))
             :window-obj huiwin/close)
     (make-main-window)))
  (hui/doui-async (/ 0))
  (hui/doui (.getUncaughtExceptionHandler (Thread/currentThread)))

  (def x (hui/doui-async (chic.windows/request-frame (first (vals @windows/*windows)))))
  (def x (hui/doui-async 5))

  @(:*ctx (first (vals @windows/*windows)))
  (:*ui-error (second (vals @windows/*windows)))
  (@*app-root)
  (io.github.humbleui.protocols/-draw (build-app-root))

  (System/gc)
  ;; a single scope/chain for shortcuts/key events where there can only be one handler at most.
  ;; widgets register/deregister their shortcuts as they come into/out of focus
  ;;   or reg/dereg upon mount/unmount and pred includes whether it is focussed.

  (.write (get @(:session (val (first @cider.nrepl.middleware.out/tracked-sessions-map)))
               #'*out*)
          "xyz")
  (.println (System/out) "x")
  (.println *out* "x")
  (require 'chic.main :reload-all)

  ;; (hui/doui (alter-var-root #'clojure.core/*warn-on-reflection* (fn [_] true)))
#!
  )
