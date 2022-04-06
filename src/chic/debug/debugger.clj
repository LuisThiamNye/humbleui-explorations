(ns chic.debug.debugger
  (:require
   [clojure.string :as str]
   [chic.ui :as cui]
   [chic.debug.live-debugger :as live-debugger]
   [chic.style :as style]
   [chic.error.stacktrace :as error.stacktrace]
   [chic.debug :as debug]
   [chic.ui.error :as cui.error]
   [chic.ui.event :as uievt]
   [clojure.reflect :as reflect]
   [chic.digger :as digger]
   [chic.digger.inspector.throwable :as inspector.throwable]
   [chic.digger.inspector.thread-shapshot :as inspector.thread-shapshot]
   [io.github.humbleui.profile :as profile]
   [chic.ui.icons.material :as maticons]
   [io.github.humbleui.paint :as huipaint]
   [chic.ui.layout :as cuilay]
   [chic.ui.svg :as ui.svg]
   [chic.windows :as windows]
   [clojure.math :as math]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.ui :as ui]
   [io.github.humbleui.window :as huiwin]
   [taoensso.encore :as enc])
  (:import
   (java.lang.reflect Method Field)
   [io.github.humbleui.skija Paint Shader Canvas]
   [io.github.humbleui.types IPoint Point Rect]))

;; (def *errors (atom {}))
(def *debuggers "by thread name" (atom {}))

(def ^:private *error-id-counter (atom Long/MIN_VALUE))

(defn new-error-id [] (swap! *error-id-counter inc))
(.hashCode(Exception.))

(def *state (atom {:selected-error-idx nil
                   :error-stack []}))
(def *throwables (atom #{}))

(defn debug-error [^Throwable throwable]
  (when-not (or (nil? debug/vm) (contains? @*throwables throwable))
    (swap! *throwables conj throwable)
    (let [thread (Thread/currentThread)]
     @(future
        (let [time (java.util.Date.)
              thread-name (.getName thread)
              thread-snap (debug/snapshot-current-thread)
              item {:throwable throwable
                    :time time
                    :thread thread
                    :thread-snapshot thread-snap}]
          (swap! *state update :error-stack conj item))
        #_(let [;id (new-error-id)
                time (java.util.Date.)
                thread-name (.getName thread)
                thread-ref (debug/vm-thread-named debug/vm thread-name)
                _ (.suspend thread-ref)
                item {:throwable throwable
                      :time time
                      :thread thread
                      :thread-ref thread-ref}]
            (swap! *debuggers assoc thread-name item)
            (swap! *state update :error-stack conj item)))))
  (throw throwable))

(defn install-debug-ctx! []
  (alter-var-root
   #'debug/*debug-ctx*
   (fn [debug-ctx]
     (assoc debug-ctx ::debug/debug-error #'debug-error))))

(defn ensure-good-select-idx* [{:keys [selected-error-idx] :as sm}]
  (let [idx (min (max selected-error-idx 0) (dec (count (:error-stack sm))))]
    (assoc sm :selected-error-idx (if (neg? idx) nil idx))))

(defn close-error-item [item]
  (swap! *throwables disj (:throwable item))
  (when-let [digger (:digger item)]
    (digger/close-session digger))
  (when-let [thread-ref ^com.sun.jdi.ThreadReference (:thread-ref item)]
    (.resume thread-ref)
    (swap! *debuggers dissoc (.name thread-ref))))

(defn control-bar []
  (ui/dynamic
   ctx [{:keys [font-ui fill-text]} ctx]
   (let [button (fn [f child]
                  (cui/clickable
                   (uievt/on-primary-down f)
                   (cuilay/padding
                    2 (ui/fill
                       (huipaint/fill 0x20000000)
                       (cuilay/padding
                        7 5 child)))))]
     (cuilay/row
      (button (fn [_]
                (swap! *state
                       (fn [{:keys [error-stack] :as sm}]
                         (if (pos? (count error-stack))
                           (assoc sm :selected-error-idx 0)
                           sm))))
              (ui/label "|<" font-ui fill-text))
      (button (fn [_]
                (swap! *state
                       (fn [{:keys [selected-error-idx error-stack] :as sm}]
                         (let [new-idx (dec (or selected-error-idx (count error-stack)))]
                           (cond-> sm
                             (<= 0 new-idx)
                             (assoc :selected-error-idx new-idx))))))
              (ui/label "<" font-ui fill-text))
      (button (fn [_]
                (swap! *state
                       (fn [{:keys [selected-error-idx error-stack] :as sm}]
                         (let [new-idx (inc (or selected-error-idx -1))]
                           (cond-> sm
                             (< new-idx (count error-stack))
                             (assoc :selected-error-idx new-idx))))))
              (ui/label ">" font-ui fill-text))
      (button (fn [_]
                (swap! *state
                       (fn [{:keys [error-stack] :as sm}]
                         (if (pos? (count error-stack))
                           (assoc sm :selected-error-idx (dec (count error-stack)))
                           sm))))
              (ui/label ">|" font-ui fill-text))
      [:stretch 1
       (cuilay/valign
        0.5 (cuilay/padding
             5 0 (ui/dynamic
                  _ [{:keys [selected-error-idx error-stack]} @*state]
                  (cuilay/row
                   (ui/label (str (if selected-error-idx (inc selected-error-idx) 0)
                                  "/" (count error-stack))
                             font-ui fill-text)
                   (ui/gap 8 0)
                   (when selected-error-idx
                     (ui/label (str (:throwable (nth error-stack selected-error-idx)))
                               font-ui fill-text))))))]
      (button
       (fn [_]
         (swap! *state
                (fn [{:keys [selected-error-idx error-stack] :as sm}]
                  (if selected-error-idx
                    (-> sm
                        (assoc :error-stack
                               (into []
                                     (comp
                                      (map-indexed (fn [i x]
                                                     (if (== selected-error-idx i)
                                                       (do (close-error-item x) nil)
                                                       x)))
                                      (remove nil?))
                                     error-stack))
                        (assoc :selected-error-idx (dec selected-error-idx))
                        (ensure-good-select-idx*))
                    sm))))
       (ui/label "Clear" font-ui fill-text))
      (button
       (fn [_]
         (swap! *state
                #(-> %
                     (cond-> (pos? (count (:error-stack %)))
                       (-> (assoc :error-stack
                                  (into []
                                        (comp
                                         (map-indexed
                                          (fn [i x]
                                            (if (== (:selected-error-idx %) i)
                                              x
                                              (do (close-error-item x) nil))))
                                         (remove nil?))
                                        (:error-stack %)))
                           (assoc :selected-error-idx 0))))))
       (ui/label "Clear rest" font-ui fill-text))))))

(defn build-main-error-window-root []
  (when (nil? (:selected-error-idx @*state))
    (swap! *state
           (fn [{:keys [selected-error-idx error-stack] :as sm}]
             (if (and (nil? selected-error-idx) (pos? (count error-stack)))
               (assoc sm :selected-error-idx (dec (count error-stack)))
               sm))))
  (ui/dynamic
   ctx [{:keys [font-ui fill-text scale]} ctx]
   (ui/with-context
     {}
     (cuilay/column
      (cui/dyncomp (control-bar))
      [:stretch 1
       (cui.error/bound-errors
        (ui/dynamic
         _ [{:keys [selected-error-idx error-stack]} @*state]
         (if-let [selected-error (and selected-error-idx (nth error-stack selected-error-idx nil))]
           (let [session (or (:digger-session selected-error)
                             (let [session (digger/new-session (assoc (:thread-snapshot selected-error)
                                                                      :throwable (:throwable selected-error)))]
                               (swap! *state assoc-in [:error-stack selected-error-idx :digger-session]
                                      session)
                               session))]
             (cuilay/column
              (cuilay/padding
               5 (ui/label (str "At: " (:time selected-error)
                                " on thread: " (some-> ^Thread (:thread selected-error) .getName)) font-ui fill-text))
              [:stretch 1 (cui/dyncomp #_(live-debugger/make selected-error)
                                       (digger/make session))]))
           (ui/gap 0 0))))]))))

(def *main-error-window-root (volatile! nil))

(defn make-main-error-window [id]
  (let [screen (last (hui/screens))
        scale (:scale screen)
        width (* 800 scale)
        height (* 600 scale)
        area (:work-area screen)
        x (:x area)
        y (-> (:height area) (- height))]
    (doto
     (windows/make
      {:id id
       :*app-root *main-error-window-root
       :build-app-root #(cui/dyncomp (build-main-error-window-root))
       :on-close #(fn [])})
      (huiwin/set-title "Errors")
      (huiwin/set-window-size width height)
      (huiwin/set-window-position x y)
      (huiwin/set-visible true))))

(def *main-error-window-agent (agent "errors" :error-mode :continue))

(defn show-error-window []
  (send *main-error-window-agent
        (fn [id]
          (if-let [existing (get @windows/*windows id)]
            (windows/activate-window existing)
            (windows/dosendui (make-main-error-window id))))))

(comment
  (send *main-error-window-agent
        (fn [id]
          (if-let [existing (get @windows/*windows id)]
            existing
            id)))
  (agent-error *main-error-window-agent)
  ()
  (windows/safe-doui (windows/on-ui-thread?))
  (get @windows/*windows "errors")

  #!
  )
