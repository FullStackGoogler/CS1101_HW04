;Eric Zhou, ezzhou
;BSL w/ List Abbreviations

; (1.)

; A Binary Search Tree (BST) is one of
; #false
; Order 
; 
; INVARIANT:
;  -key > all keys in left subtree
;  -key < all keys in right subtree
;  -All key values are unique


(define-struct order (num name credit-card loi before after))
; A Order is a (make-order Number String String ListOfItem TreeNode TreeNode)
; num is the order's number
; name is the customer's name
; credit-card is the customer's credit card
; loi is all the items ordered
; before is the order number before it
; after is the order number after it
; 
; A TreeNode is one of
; -false
; -Order
; 
; A ListOfItem is one of
; -empty
; -Item


(define-struct item (num description amount price))
; A item is a (make-item Number String Number Number)
; num is the item's number
; description is a description of what the item is
; amount is the quantity ordered
; price is the price per item


; (2.)

(define AppleWatch (make-item 376 "Apple Watch Series 6" 1 399))
(define SennHeiser (make-item 263 "SennHeiser wireless earbuds" 3 299))
(define Nvidia3090 (make-item 149 "RTX 3090 Q4 2020" 1 1499))
(define Sponges (make-item 654 "Yellow Rectangular Sponges" 20 5.99))
(define Apples (make-item 4840 "Red Delicious" 50 1.99))
(define Banana (make-item 3441 "Banana." 256 0.99))
(define Starburst (make-item 124 "Original Starburst flavors" 2048 0.25))

(define List1 (list AppleWatch SennHeiser Nvidia3090))
(define List2 (list Apples Banana Starburst))
(define List3 (list AppleWatch Apples Sponges Starburst))
(define List4 (list Apples))
(define List5 (list))

(define ORDERTREE
  (make-order 56 "Matt" "4400-8884-5525-1079" List2
              (make-order 37 "Arianna" "7764-3368-5863-2727" List4 #false #false)
              (make-order 69 "Cole" "1234-5678-9012-3456" List3
                          (make-order 63 "Drake" "7777-7777-7777-7777" List1 #false #false)
                          (make-order 89 "James" "7943-5748-3759-9599" List5 #false #false))))

; (3.)

; BST-fcn: BST -> ???
; (define (fcn-for-BinarySearchTree a-tree)
;   (cond [(boolean? a-tree) (... )]
;         [(order?  a-tree)  (... (order-num a-tree)
;                                 (order-name a-tree)
;                                 (order-credit-card a-tree)
;                                 (order-loi a-tree)
;                                 (fcn-for-BinaryTree (order-before a-tree))
;                                 (fcn-for-BinaryTree (order-after a-tree)))]))


; loi-fcn: ListOfItems -> ???
; (define (loi-fcn a-loi)
;   (cond [(empty? a-loi) (...)]
;         [(cons? a-loi)  (... (first a-loi)
;                              (a-loi-fcn (rest a-loi)))]))

; item-fcn: Item -> ???
; (define (fcn-for-item a-item)
;   (... (item-num         a-item)
;        (item-description a-item)
;        (item-amount      a-item)
;        (item-price       a-item)))

; (4.)
  
;Signature: BinarySearchTree Number -> Number
;Purpose: To calculate the total cost of all items in a order, returns -1 if no order is found
(define (order-cost a-tree order-id)
  (cond
    [(boolean? a-tree) -1]
    [else
     (cond
       [(= order-id (order-num a-tree)) (list-cost (order-loi a-tree))]
       [(< order-id (order-num a-tree)) (order-cost (order-before a-tree) order-id)]
       [(> order-id (order-num a-tree)) (order-cost (order-after a-tree) order-id)])]))

(check-expect (order-cost ORDERTREE 37) 99.5)
(check-expect (order-cost ORDERTREE 63) 2795)
(check-expect (order-cost ORDERTREE 89) 0)
(check-expect (order-cost ORDERTREE 128) -1)

;Signature: ListOfItem -> Number
;Purpose: to calculate the total cost of all items in a list of items
(define (list-cost a-loi)
  (cond
    [(empty? a-loi) 0]
    [(cons? a-loi) (+ (* (item-amount (first a-loi)) (item-price (first a-loi))) (list-cost (rest a-loi)))]))

(check-expect (list-cost List1) 2795)
(check-expect (list-cost List4) 99.5)
(check-expect (list-cost List5) 0)

; (5.)

;Signature: BinarySearchTree Number -> BinarySearchTree
;Purpose: Removes a item from all orders in a BinaryTree of orders
(define (remove-item-from-all-orders a-tree item-id)
  (cond
    [(boolean? a-tree) #false]
    [(order? a-tree) (make-order (order-num a-tree) (order-name a-tree) (order-credit-card a-tree) (remove-item (order-loi a-tree) item-id) (remove-item-from-all-orders (order-before a-tree) item-id) (remove-item-from-all-orders (order-after a-tree) item-id))]))

(check-expect (remove-item-from-all-orders ORDERTREE 4840)
              (make-order 56 "Matt" "4400-8884-5525-1079" (list Banana Starburst)
                          (make-order 37 "Arianna" "7764-3368-5863-2727" empty #false #false)
                          (make-order 69 "Cole" "1234-5678-9012-3456" (list AppleWatch Sponges Starburst)
                                      (make-order 63 "Drake" "7777-7777-7777-7777" List1 #false #false)
                                      (make-order 89 "James" "7943-5748-3759-9599" List5 #false #false))))
(check-expect (remove-item-from-all-orders ORDERTREE 376)
              (make-order 56 "Matt" "4400-8884-5525-1079" List2
                          (make-order 37 "Arianna" "7764-3368-5863-2727" List4 #false #false)
                          (make-order 69 "Cole" "1234-5678-9012-3456" (list Apples Sponges Starburst)
                                      (make-order 63 "Drake" "7777-7777-7777-7777" (list SennHeiser Nvidia3090) #false #false)
                                      (make-order 89 "James" "7943-5748-3759-9599" List5 #false #false))))
(check-expect (remove-item-from-all-orders ORDERTREE 2147483647) ORDERTREE)

;Signature: ListOfItem Number -> ListOfItem
;Purpose: returns a ListOfItem that has a specified item removed
(define (remove-item a-loi item-id)
  (cond
    [(empty? a-loi) empty]
    [(cons? a-loi) (if (= (item-num (first a-loi)) item-id)
                       (remove-item (rest a-loi) item-id)
                       (cons (first a-loi) (remove-item (rest a-loi) item-id)))]))
                   
(check-expect (remove-item List1 376) (list (make-item 263 "SennHeiser wireless earbuds" 3 299) (make-item 149 "RTX 3090 Q4 2020" 1 1499)))
(check-expect (remove-item List4 4840) empty)
(check-expect (remove-item List3 263) List3)

; (6.)

;Signature: BinarySearchTree -> ListOfNumber
;Purpose: Produces a list of the order numbers sorted in ascending order
(define (list-sorted-order-numbers a-tree)
  (cond 
    [(boolean? a-tree) empty]
    [(order? a-tree) (append (list-sorted-order-numbers (order-before a-tree)) (cons (order-num a-tree) empty) (list-sorted-order-numbers (order-after a-tree)))]))

(check-expect (list-sorted-order-numbers ORDERTREE) (list 37 56 63 69 89))
(check-expect (list-sorted-order-numbers
               (make-order 56 "Matt" "4400-8884-5525-1079" List2
                          (make-order 37 "Arianna" "7764-3368-5863-2727" List4 #false #false)
                          (make-order 69 "Cole" "1234-5678-9012-3456" List3
                                      (make-order 63 "Drake" "7777-7777-7777-7777" List1 #false #false)
                                      (make-order 89 "James" "7943-5748-3759-9599" List5 #false
                                                  (make-order 96 "Sammy" "5983-8845-6598-9189" List1 #false #false)))))
              (list 37 56 63 69 89 96))
(check-expect (list-sorted-order-numbers
               (make-order 56 "Matt" "4400-8884-5525-1079" List2
                          (make-order 37 "Arianna" "7764-3368-5863-2727" List4
                                      (make-order 18 "Jonathon" "2509-9130-4934-1218" List5 #false #false)
                                      (make-order 43 "Eric" "1203-9138-0935-3141" List1 #false #false))
                          (make-order 69 "Cole" "1234-5678-9012-3456" List3
                                      (make-order 63 "Drake" "7777-7777-7777-7777" List1 #false #false)
                                      (make-order 89 "James" "7943-5748-3759-9599" List5
                                                  (make-order 73 "Zaq" "5469-8123-0459-6776" List3 #false
                                                              (make-order 81 "James" "1277-2977-1967-1969" List2 #false #false))
                                                  #false))))
               (list 18 37 43 56 63 69 73 81 89))
 
; (7.)

;Signature: BinarySearchTree Number String String ListOfItem -> BinarySearchTree
;Purpose: Creates a new order and adds it into the existing BinarySearchTree
(define (add-new-order a-tree order-id name credit-card a-loi)
  (cond
    [(boolean? a-tree) (make-order order-id name credit-card a-loi #false #false)]
    [(order? a-tree) (if (< order-id (order-num a-tree))
                         (make-order (order-num a-tree) (order-name a-tree) (order-credit-card a-tree) (order-loi a-tree) (add-new-order (order-before a-tree) order-id name credit-card a-loi) (order-after a-tree))
                         (make-order (order-num a-tree) (order-name a-tree) (order-credit-card a-tree) (order-loi a-tree) (order-before a-tree) (add-new-order (order-after a-tree) order-id name credit-card a-loi)))]))

(check-expect (add-new-order ORDERTREE 96 "Sammy" "5983-8845-6598-9189" List1)
              (make-order 56 "Matt" "4400-8884-5525-1079" List2
                          (make-order 37 "Arianna" "7764-3368-5863-2727" List4 #false #false)
                          (make-order 69 "Cole" "1234-5678-9012-3456" List3
                                      (make-order 63 "Drake" "7777-7777-7777-7777" List1 #false #false)
                                      (make-order 89 "James" "7943-5748-3759-9599" List5 #false
                                                  (make-order 96 "Sammy" "5983-8845-6598-9189" List1 #false #false)))))
(check-expect (add-new-order ORDERTREE 16 "Robert" "5698-2333-4583-5238" List5)
              (make-order 56 "Matt" "4400-8884-5525-1079" List2
                          (make-order 37 "Arianna" "7764-3368-5863-2727" List4
                                      (make-order 16 "Robert" "5698-2333-4583-5238" List5 #false #false)
                                      #false)
                          (make-order 69 "Cole" "1234-5678-9012-3456" List3
                                      (make-order 63 "Drake" "7777-7777-7777-7777" List1 #false #false)
                                      (make-order 89 "James" "7943-5748-3759-9599" List5 #false #false))))
(check-expect (add-new-order ORDERTREE 73 "Zaq" "5469-8123-0459-6776" List3)
              (make-order 56 "Matt" "4400-8884-5525-1079" List2
                          (make-order 37 "Arianna" "7764-3368-5863-2727" List4 #false #false)
                          (make-order 69 "Cole" "1234-5678-9012-3456" List3
                                      (make-order 63 "Drake" "7777-7777-7777-7777" List1 #false #false)
                                      (make-order 89 "James" "7943-5748-3759-9599" List5
                                                  (make-order 73 "Zaq" "5469-8123-0459-6776" List3 #false #false)
                                                  #false))))
