-- Лабораторна робота №3
-- студента групи КН-32
-- підгрупа 1
-- Евдокимов Нікіта
-- Варіант №11

-- Мета: Набути досвiду визначення та використання функцій вищого порядку.

-- Завдання: Визначте вказанi функцiї в кожному з завдань: а) без застосування, б) з
-- застосуванням вбудованих функцiй вищого порядку. 

-- 1.11 Замiнити кожен n-й елемент списку вказаним значенням, напр. при
-- n=2 та значеннi ’z’: "1234590"⇒ "1z3z5z0".

-- 2.11 Знайти два простi числа, сума яких дорiвнює заданому парному N ≥
-- 2 (бiнарна проблема/гiпотеза Ґольдбаха).

-- Тести:
-- *Main> funk1 "fgjfd;gkj" 'n'
-- "fnjndngn"

-- *Main> funk2 "fgjfd;gkj" 3 'z'
-- "fgzfdzgkz"

-- *Main> funk3 5
-- (2,3)

-- *Main> funk4 6
-- (5,1)

funk1 :: String  -> Char -> String
funk1 [] ch = []
funk1 (x:xs) ch
    | length xs > 1 = x : ch : funk1 (tail xs) ch
    | otherwise = funk1 [] ch

funk2 :: String -> Int -> Char -> String
funk2 [] 0 ch = []
funk2 str n ch = helper str n 1 ch
helper [] a d f = []
helper (x:xs) n i ch = if mod i n == 0
    then ch : helper xs n (i + 1) ch
    else x : helper xs n (i + 1) ch

funk3 :: Integer -> (Integer, Integer)
funk3 1 = (0, 0)
funk3 n = helper2 n 1 1
helper2 n x1 x2
    | x1 + x2 == n = (x1, x2)
    | x1 <= x2 = helper2 n (helper3 x1) 1
    | otherwise = helper2 n x1 (helper3 x1)

helper3 n = if prime (n + 1)
    then n + 1
    else helper3 (n + 1)

prime :: (Integral a) => a -> Bool
prime 1 = True
prime x = and [ x `mod` y /= 0 | y <- [2..(x-1)] ]

-- Висновок: В результаті виконання лабораторної роботи, ми набули досвіду визначення
-- та використання функцій вищого порядку.