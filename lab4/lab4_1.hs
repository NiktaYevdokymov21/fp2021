-- Лабораторна робота №4
-- студента групи КН-32
-- підгрупа 1
-- Євдокимов Нікіта
-- Варіант №11

-- Мета: Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення
-- нових типiв та класiв типiв i їх використання.

-- Тести: 
-- *Main> funk1 [(Alert "t" "f")] "t"
-- [Alert "t" "f"]


data Notebook
    = Phone String String [String]
   | Alert String String 
   | Mettings String String String String
  deriving (Eq, Show)

funk1 :: [Notebook] -> String -> [Notebook]
funk1 [] k = []
funk1 ((Phone x y [i]) : fs) k = if x == k
    then (Phone x y [i]) : funk1 fs k
    else funk1 fs k
funk1 ((Alert x y) : fs) k = if x == k 
    then (Alert x y) : funk1 fs k
    else funk1 fs k
funk1 ((Mettings a b c d) : fs) k = funk1 fs k

-- Висновок: Під час виконання лабораторної роботи, ми ознайомилися та 
-- імплементували класи типів мови Haskell. Також ознайомилися з системою
-- типів та класів типів, визначили власні функції для нового типу. 
