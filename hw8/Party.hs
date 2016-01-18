module Party where

 import Employee
 import Data.Tree
 import Data.List

--Exercise 1

 glCons :: Employee -> GuestList -> GuestList
 glCons emp@(Emp { empFun = funVal }) (GL empList totFun) = (GL (emp : empList) (funVal + totFun))

 moreFun :: GuestList -> GuestList -> GuestList
 moreFun = max

 instance Monoid GuestList where
 	mempty = (GL [] 0)
 	mappend (GL lstEmp1 funVal1) (GL lstEmp2 funVal2) = (GL (lstEmp1 ++ lstEmp2) (funVal1 + funVal2))
 	--mappend (GL lstEmp funVal) gl2 = foldr (\emp acc ->(glCons emp acc)) gl2 lstEmp

--Exercise 2
 treeFold ::  (a -> [b] -> b) -> Tree a -> b
 treeFold f (Node a []) = f a []
 treeFold f (Node a subTrees) = f a (map (treeFold f) subTrees)

 --combineGLs :: Employee -> [GuestList] -> GuestList
 nextLevel :: Employee -> [(GuestList , GuestList)] -> (GuestList,GuestList)
 nextLevel boss [] = ((GL [boss] (empFun boss)),mempty)
 nextLevel boss someList = 
 	( 
 		foldr (mappend) (GL [boss] (empFun boss)) (map snd someList),
 		foldr (mappend) (mempty) (map (\x -> (max (fst x) (snd x))) someList)
 	 )
 
 maxFun :: Tree Employee -> GuestList
 maxFun company =  max (fst guestList) (snd guestList) where guestList = treeFold nextLevel company

 main :: IO ()
 main = readFile "company.txt" >>= (\companyString -> guestListToIO (maxFun (read companyString)))

 guestListToIO :: GuestList -> IO ()
 guestListToIO (GL empList funVal) = putStrLn ("Total Fun val is :: " ++ (show funVal)) >> 
 							putStrLn (foldr (\x acc -> x ++ "\n" ++ acc) "" (sort (map (empName) empList)))


    