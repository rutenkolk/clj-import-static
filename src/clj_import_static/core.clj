(ns clj-import-static.core)

(defn- macroexpand-n [n s-expr]
  (if (< n 1)
    (macroexpand-1 s-expr)
    (macroexpand-n (dec n) (macroexpand-1 s-expr))))
(defn- macroexpand-list [n s-expr]
  (map #(macroexpand-n % s-expr) (range n)))
(defn- macroexpand-readable-list [n s-expr]
  (interpose (apply str (repeat 80 "-")) (macroexpand-list n s-expr)))
(defn- identity-println [v s]
  (println s)
  v)

(defn- all-public-static-class-methods [clz]
  (set (filter
        (fn [method] (and
                      (java.lang.reflect.Modifier/isPublic (.getModifiers method))
                      (java.lang.reflect.Modifier/isStatic (.getModifiers method))))
        (.getMethods clz))))

(defn- all-public-static-class-fields [clz]
  (set (filter
        (fn [method] (and
                      (java.lang.reflect.Modifier/isPublic (.getModifiers method))
                      (java.lang.reflect.Modifier/isStatic (.getModifiers method))))
        (.getFields clz))))

(comment
  (all-public-static-class-methods java.lang.Math)

  (all-public-static-class-fields java.lang.Math)
)

(defmacro def- [sym-name value]
  `(def ~(with-meta sym-name {:private true})
     ~value))

(defmacro import-static-all-once
  "static imports ALL methods and fields of a java class.
  tries to emulate an \"import static CLASSNAME.*\".
  This will define private clojure functions which wrap the static java method, and define private vars for all static fields in a class, assuming them to be constants.
  Conflicting bindings are ignored. For example:
  If you already have a var \"foo\" bound, CLASSNAME/foo will not overwrite foo.

  Will generate a multiple-arity function var for static methods with different arity versions.
  For example: java.lang.Character/codePointAt has 3 versions. One version has 2 parameters and two other versions have 3 parameters and differ only in the input types.
  This macro will define one codePointAt function-var with 2 different arities:

  (defn-
    codePointAt
    ([G__11919 G__11920] (java.lang.Character/codePointAt G__11919 G__11920))
    ([G__11921 G__11922 G__11923] (java.lang.Character/codePointAt G__11921 G__11922 G__11923)))
  "
  [input-class-symbol]
  (let [clz (eval input-class-symbol)
        mthds (all-public-static-class-methods clz)
        filtered-methods (filter #(not (resolve (symbol (.getName %)))) mthds)
        grouped-mthds (map
                  (fn [[m-name l]] (group-by #(count (.getParameterTypes %)) l))
                  (group-by #(.getName %) filtered-methods))
        fields (all-public-static-class-fields clz)
        mthd-defs (map (fn [mem]
                         (let [mem-name (.getName (first (first (vals mem))))
                               full-name (str (.getName clz) "/" mem-name)
                               arities (map (fn [[num-count [x & dc]]]
                                              (let [params (map (fn [_] (gensym)) (range num-count))
                                                    body (concat (list (symbol full-name)) params)
                                                    ]
                                              (list (vec params) body)))
                                            mem)]
                           (list 'fn [] (concat (list 'defn-) [(symbol mem-name)] arities))))
                       grouped-mthds
                       )
        field-defs (map (fn [mem]
                         (let [body (symbol (str (.getName clz) "/" (.getName mem)))  ]
                                 (list 'fn [] (list 'def- (symbol (.getName mem)) body))))
                       (filter #(not (resolve (symbol (.getName %)))) fields))
        all-defs (concat mthd-defs field-defs)
        ]
      `(reduce #(%2) nil ~(vec all-defs))))

(defmacro import-static-all
  "static imports ALL methods and fields of a java class.
  tries to emulate an \"import static CLASSNAME.*\".
  This will define private clojure functions which wrap the static java method, and define private vars for all static fields in a class, assuming them to be constants.
  Overwrites existing bindings For example:
  If you already have a var \"foo\" bound, CLASSNAME/foo WILL overwrite foo.

  Will generate a multiple-arity function var for static methods with different arity versions.
  For example: java.lang.Character/codePointAt has 3 versions. One version has 2 parameters and two other versions have 3 parameters and differ only in the input types.
  This macro will define one codePointAt function-var with 2 different arities:

  (defn-
    codePointAt
    ([G__11919 G__11920] (java.lang.Character/codePointAt G__11919 G__11920))
    ([G__11921 G__11922 G__11923] (java.lang.Character/codePointAt G__11921 G__11922 G__11923)))
  "
  [input-class-symbol]
  (let [clz (eval input-class-symbol)
        mthds (all-public-static-class-methods clz)
        filtered-methods mthds
        grouped-mthds (map
                  (fn [[m-name l]] (group-by #(count (.getParameterTypes %)) l))
                  (group-by #(.getName %) filtered-methods))
        fields (all-public-static-class-fields clz)
        mthd-defs (map (fn [mem]
                         (let [mem-name (.getName (first (first (vals mem))))
                               full-name (str (.getName clz) "/" mem-name)
                               arities (map (fn [[num-count [x & dc]]]
                                              (let [params (map (fn [_] (gensym)) (range num-count))
                                                    body (concat (list (symbol full-name)) params)
                                                    ]
                                              (list (vec params) body)))
                                            mem)]
                           (list 'fn [] (concat (list 'defn-) [(symbol mem-name)] arities))))
                       grouped-mthds
                       )
        field-defs (map (fn [mem]
                         (let [body (symbol (str (.getName clz) "/" (.getName mem)))  ]
                                 (list 'fn [] (list 'def- (symbol (.getName mem)) body))))
                        fields)
        all-defs (concat mthd-defs field-defs)
        ]
      `(reduce #(%2) nil ~(vec all-defs))))

(defmacro import-static-all-once-multiple [clzs]
  (let [classes (if (symbol? clzs) (eval clzs) clzs)]
    `(reduce #(%2) nil ~(vec (map #(list 'fn [] (list 'import-static-all-once %)) classes)))))

(defmacro import-static-all-multiple [clzs]
  (let [classes (if (symbol? clzs) (eval clzs) clzs)]
    `(reduce #(%2) nil ~(vec (map #(list 'fn [] (list 'import-static-all %)) classes)))))

(defmacro import-static
  "static imports all SPECIFIED methods and fields of a java class.
  tries to emulate an \"import static CLASSNAME.SOME_THING\".
  This will define private clojure functions which wrap the static java method, and define private vars for all static fields in a class, assuming them to be constants.
  Overwrites existing bindings For example:
  If you already have a var \"foo\" bound, CLASSNAME/foo WILL overwrite foo.

  Will generate a multiple-arity function var for static methods with different arity versions.
  For example: java.lang.Character/codePointAt has 3 versions. One version has 2 parameters and two other versions have 3 parameters and differ only in the input types.
  This macro will define one codePointAt function-var with 2 different arities:

  (defn-
    codePointAt
    ([G__11919 G__11920] (java.lang.Character/codePointAt G__11919 G__11920))
    ([G__11921 G__11922 G__11923] (java.lang.Character/codePointAt G__11921 G__11922 G__11923)))
  "
  [input-class-symbol & member-symbols]
  (let [clz (eval input-class-symbol)
        mthds (all-public-static-class-methods clz)
        filtered-methods (filter #(some #{(symbol (.getName %))} member-symbols) mthds)
        grouped-mthds (map
                  (fn [[m-name l]] (group-by #(count (.getParameterTypes %)) l))
                  (group-by #(.getName %) filtered-methods))
        fields (all-public-static-class-fields clz)
        mthd-defs (map (fn [mem]
                         (let [mem-name (.getName (first (first (vals mem))))
                               full-name (str (.getName clz) "/" mem-name)
                               arities (map (fn [[num-count [x & dc]]]
                                              (let [params (map (fn [_] (gensym)) (range num-count))
                                                    body (concat (list (symbol full-name)) params)
                                                    ]
                                              (list (vec params) body)))
                                            mem)]
                           (list 'fn [] (concat (list 'defn-) [(symbol mem-name)] arities))))
                       grouped-mthds
                       )
        field-defs (map (fn [mem]
                         (let [body (symbol (str (.getName clz) "/" (.getName mem)))  ]
                                 (list 'fn [] (list 'def- (symbol (.getName mem)) body))))
                        (filter #(some #{(symbol (.getName %))} member-symbols) fields))
        all-defs (concat mthd-defs field-defs)
        ]
      `(reduce #(%2) nil ~(vec all-defs))))

(defmacro import-static-macro
  "static imports all SPECIFIED methods and fields of a java class.
  tries to emulate an \"import static CLASSNAME.SOME_THING\".

  This is more like the clojure.contrib/import-static behaviour.
  Instead of defining a function var, for the imported method or variable, it defines a macro.
  This may be faster and play more niceley with gen-class.
  Does not expand to a reduce that defines functions but rather a (do ) block which contains the defmacros for methods and private defs for fields.
  "
  [input-class-symbol & member-symbols]
  (let [clz (eval input-class-symbol)
        mthds (all-public-static-class-methods clz)
        filtered-methods (filter #(some #{(symbol (.getName %))} member-symbols) mthds)
        grouped-mthds (map
                  (fn [[m-name l]] (group-by #(count (.getParameterTypes %)) l))
                  (group-by #(.getName %) filtered-methods))
        fields (all-public-static-class-fields clz)
        mthd-defs (map (fn [mem]
                         (let [mem-name (.getName (first (first (vals mem))))
                               full-name (str (.getName clz) "/" mem-name)
                               arities (map (fn [[num-count [x & dc]]]
                                              (let [params (map (fn [_] (gensym)) (range num-count))
                                                    body (concat (list (symbol full-name)) params)]
                                              (list (vec params) body)))
                                            mem)]
                           (concat (list 'defmacro) [(symbol mem-name)] arities)))
                       grouped-mthds
                       )
        field-defs (map (fn [mem]
                         (let [body (symbol (str (.getName clz) "/" (.getName mem)))  ]
                           (list 'def- (symbol (.getName mem)) body)))
                        (filter #(some #{(symbol (.getName %))} member-symbols) fields))
        all-defs (concat mthd-defs field-defs)
        ]
    `(do ~@all-defs)))

(comment
  (macroexpand-n 10 '(import-static-all java.lang.Character))

  (import-static-all java.lang.Math)

  (abs -3)

  abs

  (macroexpand-n 10 '(import-static java.lang.Math PI sqrt))

  (import-static java.lang.Math PI sqrt)

  (macroexpand-n 10 '(import-static-macro java.lang.Math PI sqrt))

  (import-static-macro java.lang.Math PI sqrt)

  (macroexpand-n 10 '(import-static-macro java.lang.Character codePointAt))

  )

(comment

  (macroexpand-1 '(import-static-all java.util.List))

  )
