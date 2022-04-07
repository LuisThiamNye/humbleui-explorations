(ns chic.filebwr
  (:require
   [babashka.fs :as fs]
   [chic.text-editor :as text-editor]
   [chic.ui.event :as uievt]
   [chic.text-editor.core :as text-editor.core]
   [chic.ui :as cui]
   [chic.ui.error :as cui.error]
   [chic.ui.icons.material :as maticons]
   [chic.ui.layout :as cuilay]
   [chic.ui.svg :as ui.svg]
   [chic.ui.text-input :as text-input]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.jwm MouseButton]
   [io.github.humbleui.skija Paint]))

(def *selected-file (atom nil))
(def *directory (atom (io/file ".")))

(defn editor-view []
  (ui/dynamic
   ctx [file @*selected-file
        font-ui (:font-ui ctx)
        font-code (:font-code ctx)
        fill-text (:fill-text ctx)]
   (cui.error/bound-errors
    (if file
      (if (fs/directory? file)
        (cuilay/padding
         10 10
         (cuilay/row (ui/label "Directory: " font-ui fill-text)
                 (ui/label (str file) font-code fill-text)))
        (let [content (slurp (str file))]
          (cuilay/padding
           5 5
           (if (< 100000 (count content))
             (ui/label "File too big" font-ui fill-text)
             (cuilay/vscrollbar
              (cuilay/vscroll
               (cui/dyncomp
                (text-editor/element (text-editor.core/make {:content content :pos 0})))))))))
      (ui/gap 0 0)))))

(declare file-tree-children)

(defn file-list-item [{:keys [font-ui fill-text] :as ctx} entry]
  (let [string (fs/file-name entry)
        dir? (fs/directory? entry)
        children (when dir?
                   (fs/list-dir entry))
        *menu? (volatile! false)
        offset 16
        menu-label (fn [text] (cuilay/row
                               (ui/gap offset 0)
                               (cuilay/padding
                                0 3 (ui/label text font-ui fill-text))))
        delete-button (fn []
                        (let [*expanded? (volatile! false)]
                          (ui/dynamic
                           _ [expanded? @*expanded?]
                           (cuilay/column
                            (cui/clickable
                             (fn [event]
                               (when (and (identical? MouseButton/PRIMARY (uievt/mouse-button event))
                                          (:hui.event.mouse-button/is-pressed event))
                                 (vswap! *expanded? not)))
                             (menu-label "Delete"))
                            (when expanded?
                              (cuilay/row
                               (ui/gap 20 0)
                               (cui/clickable
                                (fn [event]
                                  (let [button (uievt/mouse-button event)]
                                    (cond
                                      (and (identical? MouseButton/PRIMARY button)
                                           (:hui.event.mouse-button/is-pressed event))
                                      (if dir?
                                        (fs/delete-tree entry)
                                        (fs/delete-if-exists entry)))))
                                (menu-label "DELETE PERMANENTLY"))))))))
        el (fn [on-click]
             (ui/dynamic
              _ [selected-file @*selected-file
                 menu? @*menu?]
              (let [main-el
                    (cui/clickable
                     (fn [event]
                       (let [button (uievt/mouse-button event)]
                         (cond
                           (identical? MouseButton/PRIMARY button)
                           (when (:hui.event.mouse-button/is-pressed event)
                             (on-click))
                           (identical? MouseButton/SECONDARY button)
                           (when (:hui.event.mouse-button/is-pressed event)
                             (vswap! *menu? not)))))
                     (ui/fill
                      (doto (Paint.)
                        (.setColor (unchecked-int (if (and selected-file (= entry selected-file))
                                                    0xFFC0FF90
                                                    0x00000000))))
                      (cuilay/row
                       (cuilay/width
                        offset
                        (cuilay/padding
                         2 0
                         (cuilay/valign
                          0.5 (cuilay/height
                               12 (ui.svg/make (if dir?
                                                 (maticons/svg-data "folder" "outlined" "24px")
                                                 (maticons/svg-data "description" "outlined" "24px")))))))
                       (cuilay/padding
                        2 5
                        (ui/label string font-ui (cond-> ^Paint fill-text
                                                   (nil? entry)
                                                   (-> .makeClone (doto (.setAlpha (unchecked-int 0x90))))))))))]
                (if menu?
                  (cuilay/column
                   main-el
                   (delete-button))
                  main-el))))]
    (if (fs/directory? entry)
      (let [*expanded? (volatile! false)
            theel (el #(vswap! *expanded? not))]
        (ui/dynamic
         _ [expanded? @*expanded?]
         (cuilay/column
          theel
          (when (and expanded? children)
            (cuilay/row
             (ui/gap 10 0)
             [:stretch 1
              (cuilay/column
               (cui/dyncomp
                (file-tree-children ctx children)))])))))
      (el #(reset! *selected-file entry)))))

(defn file-tree-children [ctx children]
  (cuilay/column
   (eduction
    (remove (fn [file]
              (let [basename (fs/file-name file)]
                (or (str/starts-with? basename ".#")
                    (= ".DS_Store" basename)
                    (str/ends-with? basename "~undo-tree~")))))
    (map (fn [file]
           (cui/dyncomp
            (file-list-item ctx file))))
    (sort-by str children))))

(def *directory-items (atom (fs/list-dir @*directory)))

(defn file-tree-view []
  (cuilay/vscrollbar
   (cuilay/vscroll
    (ui/dynamic ctx [font-ui (:font-ui ctx)
                     fill-text (:fill-text ctx)
                     directory-items @*directory-items]
      (cuilay/column
       (cui/dyncomp
        (file-tree-children {:font-ui font-ui :fill-text fill-text} directory-items))
       #_(cuilay/padding
          20 20
          (text-editor/element editor))
       (ui/gap 0 50))))))

(defn file-tree-toolbar []
  (cuilay/column
   (cuilay/row
    (cui/clickable
     (uievt/on-primary-down (fn [_] (reset! *directory-items (fs/list-dir @*directory))))
     (cuilay/padding
      4 4 (cuilay/height
           18 (cuilay/width 18 (ui.svg/make (maticons/svg-data "refresh" "round" "24px"))))))
    (cui/clickable
     (fn [_])
     (cuilay/padding
      4 4 (cuilay/height
           18 (cuilay/width 18 (ui.svg/make (maticons/svg-data "add" "round" "24px"))))))
    [:stretch 1
     (ui/gap 0 0)])
   (text-input/make)))

(comment
  (fs/list-dir @*directory)
  #!
  )

(defn basic-view []
  (cuilay/row
   [:stretch 1
    (cuilay/column
     (cui/dyncomp (file-tree-toolbar))
     [:stretch 1
      (cui.error/bound-errors
       (cui/dyncomp (file-tree-view)))])]
   [:stretch 2 (cui/dyncomp (editor-view))]))
