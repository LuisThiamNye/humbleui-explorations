(ns chic.key
  (:import
   (io.github.humbleui.jwm EventKey KeyModifier Key)))

(defn no-modifiers? [^EventKey ek]
  ;; ignore caps lock (bitshift)
  ;; MAC_FN is activated always with the arrow keys
  (let [mods (.-_modifiers ek)]
    (== 0 (bit-shift-right
           (if (.isArrowKey (.getKey ek))
             (bit-and-not mods (.-_mask KeyModifier/MAC_FN))
             mods)
           1))))

(defn only-modifier? [^EventKey ek mod-mask]
  (let [ignore-mask (cond->
                     (.-_mask KeyModifier/CAPS_LOCK)
                      (.isArrowKey (.getKey ek))
                      (bit-or (.-_mask KeyModifier/MAC_FN)))]
    (== (bit-and-not (.-_modifiers ek) ignore-mask)
        (bit-and-not mod-mask ignore-mask))))

(defn mask [k]
  (if (instance? KeyModifier k)
    (.-_mask ^KeyModifier k)
    (.-_mask ^Key k)))

(defn combine [ks]
  (reduce (fn [acc x] (bit-or acc (mask x))) 0 ks))

(comment
  (def eventkey chic.text-editor/eventkey)
  (.-_modifiers eventkey)
  (bit-shift-right (.-_modifiers eventkey) 1)
  (identical? io.github.humbleui.jwm.Key/RIGHT (.getKey eventkey))
  (.isModifierDown eventkey io.github.humbleui.jwm.KeyModifier/CAPS_LOCK)
  (.-_mask io.github.humbleui.jwm.KeyModifier/CAPS_LOCK)

  (only-modifiers? eventkey [io.github.humbleui.jwm.KeyModifier/MAC_FN])
  (only-modifiers? eventkey [io.github.humbleui.jwm.KeyModifier/MAC_OPTION])
  (only-modifiers? eventkey [io.github.humbleui.jwm.KeyModifier/ALT])
  (bit-or 0 (.-_mask io.github.humbleui.jwm.KeyModifier/MAC_FN))
  (only-modifiers? eventkey [])
  (bit-and-not 1 0)

  #!
  )
