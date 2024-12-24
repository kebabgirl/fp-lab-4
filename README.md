<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студентка</b>: Петрук Ольга Сергіївна КВ-11</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання

Завдання складається з двох частин:

1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
   роботи 3 з такими змінами:

   - використати функції вищого порядку для роботи з послідовностями (де це доречно);
   - додати до інтерфейсу функції (та використання в реалізації) два ключових
     параметра: key та test , що працюють аналогічно до того, як працюють
     параметри з такими назвами в функціях, що працюють з послідовностями. При
     цьому key має виконатись мінімальну кількість разів.

2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
   варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за
   можливості, має бути мінімізоване.

## Варіант першої частини 3

Алгоритм сортування обміном №3 (із запам'ятовуванням місця останньої перестановки) за незменшенням.

## Лістинг реалізації першої частини завдання

```lisp
(defun bubble-step (lst limit last-swap &key (key #'identity) (test #'<))
  (if (or (null (cdr lst)) (= limit 0))
      (values lst last-swap)
      (let* ((a (car lst))
             (b (cadr lst))
             (key-a (funcall key a))
             (key-b (funcall key b)))
        (if (funcall test key-b key-a)
            (let* ((swapped (cons b (cons a (cddr lst)))))
              (multiple-value-bind (sorted-lst new-last-swap)
                  (bubble-step (cdr swapped) (1- limit) t :key key :test test)
                (values (cons (car swapped) sorted-lst)
                        (or new-last-swap (1- limit)))))
            (multiple-value-bind (sorted-lst new-last-swap)
                (bubble-step (cdr lst) (1- limit) last-swap :key key :test test)
              (values (cons a sorted-lst)
                      (or new-last-swap last-swap)))))))

(defun recursive-bubble-sort (lst &key (key #'identity) (test #'<))
  (labels ((recursive-sort (lst limit)
             (multiple-value-bind (new-lst last-swap)
                 (bubble-step lst limit nil :key key :test test)
               (if last-swap
                   (recursive-sort new-lst (1- limit))
                   new-lst))))
    (recursive-sort lst (length lst))))
```

### Тестові набори та утиліти першої частини

```lisp
(defun check-recursive-sort (test-name input expected &key (key #'identity) (test #'<))
  (let ((result (recursive-bubble-sort input :key key :test test)))
    (if (equal result expected)
        (format t "~a passed: ~a -> ~a~%" test-name input result)
        (format t "~a failed: ~a -> ~a (expected: ~a)~%" test-name input result expected))))

(defun test-recursive-sort ()
  (check-recursive-sort "Test 1" '(3 1 4 1 5 9 2 6 5 3 5) '(1 1 2 3 3 4 5 5 5 6 9))
  (check-recursive-sort "Test 2" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-recursive-sort "Test 3" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-recursive-sort "Test 4" '() '())
  (check-recursive-sort "Test 5" '(1) '(1))
  (check-recursive-sort "Test 6" '(1 2 2 1) '(1 1 2 2))

  (check-recursive-sort "Test 7" '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(-1 1 -2 3 3 -4 -5 -5 -5 6 9) :key #'abs)
  (check-recursive-sort "Test 8" '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(-1 1 -2 3 3 -4 -5 -5 -5 6 9) :key #'abs :test #'<)
  (check-recursive-sort "Test 9" '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(1 -1 -2 3 3 -4 -5 -5 -5 6 9) :key #'abs :test #'<=)

  (check-recursive-sort "Test 10" '(3 -1 -4 1 -5 9 -2 6 -5 3 -5) '(9 6 -5 -5 -5 -4 3 3 -2 -1 1) :key #'abs :test #'>)
  (check-recursive-sort "Test 11" '(3 2 5 4 1) '(1 2 3 4 5) :test #'<=)
  (check-recursive-sort "Test 12" '(3 2 5 4 1) '(5 4 3 2 1) :test #'>))
```

### Тестування першої частини

```
Test 1 passed: (3 1 4 1 5 9 2 6 5 3 5) -> (1 1 2 3 3 4 5 5 5 6 9)
Test 2 passed: (1 2 3 4 5) -> (1 2 3 4 5)
Test 3 passed: (5 4 3 2 1) -> (1 2 3 4 5)
Test 4 passed: NIL -> NIL
Test 5 passed: (1) -> (1)
Test 6 passed: (1 2 2 1) -> (1 1 2 2)
Test 7 passed: (3 -1 -4 1 -5 9 -2 6 -5 3 -5) -> (-1 1 -2 3 3 -4 -5 -5 -5 6 9)
Test 8 passed: (3 -1 -4 1 -5 9 -2 6 -5 3 -5) -> (-1 1 -2 3 3 -4 -5 -5 -5 6 9)
Test 9 passed: (3 -1 -4 1 -5 9 -2 6 -5 3 -5) -> (1 -1 -2 3 3 -4 -5 -5 -5 6 9)
Test 10 passed: (3 -1 -4 1 -5 9 -2 6 -5 3 -5) -> (9 6 -5 -5 -5 -4 3 3 -2 -1 1)
Test 11 passed: (3 2 5 4 1) -> (1 2 3 4 5)
Test 12 passed: (3 2 5 4 1) -> (5 4 3 2 1)
```

## Варіант другої частини 7

Написати функцію remove-each-nth-fn , яка має один основний параметр n та один
ключовий параметр — функцію key . remove-each-nth-fn має повернути функцію, яка
при застосуванні в якості першого аргументу remove-if робить наступне: кожен n -ний
елемент списку-аргумента remove-if , для якого функція key повертає значення t
(або не nil ), видаляється зі списку. Якщо користувач не передав функцію key у
remove-each-nth-fn , тоді зі списку видаляється просто кожен n -ний елемент.

```lisp
CL-USER> (remove-if (remove-each-nth-fn 2) '(1 2 3 4 5))
(1 3 5)
CL-USER> (remove-if (remove-each-nth-fn 2 :key #'evenp) '(1 2 2 2 3 4 4 4 5))
(1 2 2 3 4 5)
```

## Лістинг реалізації другої частини завдання

```lisp
(defun remove-each-nth-fn (n &key key)
  (let ((count 0))
    (lambda (element)
      (setf count (+ count 1))
      (if key
          (and (= (mod count n) 0) (funcall key element))
          (= (mod count n) 0)))))
```

### Тестові набори та утиліти другої частини

```lisp
(defun check-remove-each-nth-fn (test-name input n expected &key key)
  (let ((result (remove-if (remove-each-nth-fn n :key key) input)))
    (if (equal result expected)
        (format t "~a passed: ~a -> ~a~%" test-name input result)
        (format t "~a failed: ~a -> ~a (expected: ~a)~%" test-name input result expected))))

(defun test-remove-each-nth-fn ()
  (check-remove-each-nth-fn "Test 1" '(1 2 3 4 5) 2 '(1 3 5))
  (check-remove-each-nth-fn "Test 2" '(1 2 3 4 5 6) 3 '(1 2 4 5))
  (check-remove-each-nth-fn "Test 3" '(1 2 3 4) 1 '())

  (check-remove-each-nth-fn "Test 4" '(1 2 3 4) 2 '(1 3) :key #'evenp)
  (check-remove-each-nth-fn "Test 5" '(1 2 2 2 3 3 4 4 5) 3 '(1 2 2 2 3 4 4) :key #'oddp)
  (check-remove-each-nth-fn "Test 6" '(1 2 2 2 2 3 4 4 4 5) 2 '(1 2 2 3 4 4 5) :key #'evenp))
```

### Тестування другої частини

```
Test 1 passed: (1 2 3 4 5) -> (1 3 5)
Test 2 passed: (1 2 3 4 5 6) -> (1 2 4 5)
Test 3 passed: (1 2 3 4) -> NIL
Test 4 passed: (1 2 3 4) -> (1 3)
Test 5 passed: (1 2 2 2 3 3 4 4 5) -> (1 2 2 2 3 4 4)
Test 6 passed: (1 2 2 2 2 3 4 4 4 5) -> (1 2 2 3 4 4 5)
```
