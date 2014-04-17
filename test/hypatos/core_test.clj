(ns hypatos.core-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [hypatos.core :as h]))

(def c (h/connect "http://127.0.0.1:4001"))

; Delete all data before each test
(use-fixtures :each #(do (h/delete-all! c nil) (%)))

(deftest remap-keys-test
  (is (= (h/remap-keys inc {1 2 3 4})
         {2 2 4 4})))

(deftest key-encoding-test
  (testing "nil"
    (is (= "" (h/encode-key nil))))

  (testing "simple strings"
    (is (= "foo" (h/encode-key "foo"))))

  (testing "strings with slashes"
    (is (= "foo/bar" (h/encode-key "foo/bar"))))

  (testing "unicode"
    (is (= "%E2%88%B4/%E2%88%8E" (h/encode-key "∴/∎"))))

  (testing "keywords"
    (is (= "foo" (h/encode-key :foo))))

  (testing "symbols"
    (is (= "foo" (h/encode-key 'foo))))

  (testing "sequences"
    (is (= "foo"     (h/encode-key [:foo])))
    (is (= "foo/bar" (h/encode-key [:foo :bar])))
    (is (= "foo/bar" (h/encode-key '(:foo :bar))))
    (is (= "foo/bar/baz" (h/encode-key ["foo/bar" "baz"])))))

(deftest reset-get-test
  (testing "a simple key"
    (h/reset! c "test" "hi")
    (is (= "hi" (h/get c "test"))))

  (testing "Paths and unicode"
    (h/reset! c "∴/∎" "ℵ")
    (is (= "ℵ" (h/get c ['∴ '∎])))))

(deftest list-directory
  (is (= (h/get c nil)
         nil))

  (h/reset! c "foo" 1)
  (h/reset! c "bar" 2)
  (h/reset! c [:folder :of :stuff] "hi")

  (is (= (h/get c nil)
         {"foo"     "1"
          "bar"     "2"
          "folder"  nil}))

  (is (= (h/get c nil {:recursive? true})
         {"foo" "1"
          "bar" "2"
          "folder" {"of" {"stuff" "hi"}}})))

(deftest missing-values
  (is (nil? (h/get c :nonexistent))))

(deftest create-test!
  (let [r (str (rand))
        k (h/create! c :rand r)]
    (is (re-find #"^/rand/\d+$" k))
    (is (= r (h/get c k)))))

(deftest cas-test!
  (h/reset! c :foo :init)
  (is (false? (h/cas! c :foo :nope :next)))
  (is (= ":init" (h/get c :foo)))

  (h/cas! c :foo :init :next)
  (is (= ":next" (h/get c :foo))))

(deftest cas-index-test
  (let [idx (:createdIndex (:node (h/reset! c "foo" 0)))]
    (h/cas-index! c :foo idx 1)
    (is (= "1" (h/get c :foo)))

    (is (false? (h/cas-index! c :foo idx 2)))
    (is (= "1" (h/get c :foo)))))

(deftest swap-test
  (h/reset! c :atom 0)
  (is (= 3 (h/swap! c :atom (fn [a b c]
                              (+ (read-string a) b c)) 1 2)))
  (is (= "3" (h/get c :atom))))

;(deftest swap-aggressive-test
;  (h/reset! c :atom "0")
;  (->> (range 100)
;       (map #(future % (h/swap! c :atom (comp inc read-string))))
;       doall
;       (map deref)
;       sort
;       (= (range 1 101))
;       is)
;  (is (= "100" (h/get c :atom {:consistent? true}))))
