module SplitAlgorithm where

import qualified Model.Data as MD
import qualified View.State as VS
import qualified Data.Map.Strict as HashMap
import qualified View.AddExpense as AE
import Data.List as List
import Data.Ord
-- get the balance map

getTheInfoFromRecordToList :: MD.ExpenseRecord -> [(String, String, Float)]
getTheInfoFromRecordToList (MD.ExpenseRecord billingID title description creditor debtors amount createDate) = map combi debtors where
    combi :: String -> (String, String, Float)
    combi x = (creditor, x, AE.truncate' (amount/(fromIntegral (length debtors))) 2)

addOneInHashmap :: String -> HashMap.Map String Float -> Float -> HashMap.Map String Float
addOneInHashmap s oldMap amount = newMap
   where newMap = HashMap.insertWith (+) s amount oldMap

minisOneInHashmap :: String -> HashMap.Map String Float -> Float -> HashMap.Map String Float
minisOneInHashmap s oldMap amount = newMap
   where newMap = HashMap.insertWith (+) s (0 - amount) oldMap

getBalanceMapFromTupleList :: [(String, String, Float)] -> HashMap.Map String Float
getBalanceMapFromTupleList [] = HashMap.empty
getBalanceMapFromTupleList (t:ts) = updateMap t (getBalanceMapFromTupleList ts) 
    where
        updateMap tu mp = minisOneInHashmap (get1 tu) (addOneInHashmap (get2 tu) mp (get3 tu)) (get3 tu)
        get1 (a,_,_) = a
        get2 (_,b,_) = b
        get3 (_,_,c) = c

getBalanceMap :: [MD.ExpenseRecord] -> HashMap.Map String Float
getBalanceMap [] = HashMap.empty
getBalanceMap er = HashMap.filter (\x -> x>0.1) (getBalanceMapFromTupleList tupleList)
    where tupleList = concat $ map getTheInfoFromRecordToList er 

-- def getBalanceMap: 
--         balances = {}
--         for A, B, amount in transactions:
--             balances.setdefault(A, 0)
--             balances.setdefault(B, 0)
--             balances[A] -= amount
--             balances[B] += amount

--         balances = {
--             P: amount
--             for P, amount in balances.items()
--             if amount != 0}

-- ******************************************* heuristic algorithm (O(n)) *******************************************

-- getPayerMap :: HashMap.Map String Int -> HashMap.Map String Int
-- getPayerMap HashMap.empty = []
-- getPayerMap mp = HashMap.filter (\x -> x>0) mp

-- getReceiverMap :: HashMap.Map String Int -> HashMap.Map String Int
-- getReceiverMap HashMap.empty = []
-- getReceiverMap mp = HashMap.filter (\x -> x<0) mp

getSuggestionsGreedy :: [MD.ExpenseRecord] -> [MD.SplitSuggestion]
getSuggestionsGreedy [] = []
getSuggestionsGreedy rs = getSuggestionsWithMap $ getBalanceMap rs 
-- where
    -- payerMap = getPayerMap $ getBalanceMap rs
    -- receiverMap = getReceiverMap $ getBalanceMap rs

getSuggestionsWithMap :: HashMap.Map String Float -> [MD.SplitSuggestion]
getSuggestionsWithMap balanceMap = 
    if null balanceMap
        then []
    else
        (getSuggestion maxPayerTuple maxReciverTuple) : (getSuggestionsWithMap (updateBalance balanceMap maxPayerTuple maxReciverTuple))
        where
            -- (\(k1, v1) (k2, v2) -> v2 `compare` v1)
            maxPayerTuple = List.maximumBy (comparing snd) (HashMap.toList balanceMap)
            maxReciverTuple = List.minimumBy (comparing snd) (HashMap.toList balanceMap)
            getSuggestion mp mr = MD.SplitSuggestion {
            MD.debtor = fst mp,
            MD.suggestCreditor = fst mr,
            MD.suggestAmount = AE.truncate' (min (snd mp) (abs (snd mr))) 2
            }

updateBalance :: HashMap.Map String Float -> (String, Float) -> (String, Float) -> HashMap.Map String Float
updateBalance balanceMap maxPayerTuple maxReciverTuple = 
    if condition1 maxPayerTuple maxReciverTuple
        then HashMap.delete (fst maxReciverTuple) (HashMap.insertWith (+) (fst maxPayerTuple) (snd maxReciverTuple) balanceMap)
    else if condition2 maxPayerTuple maxReciverTuple
        then HashMap.delete (fst maxPayerTuple) (HashMap.insertWith (+) (fst maxReciverTuple) (snd maxPayerTuple) balanceMap)
    else
        HashMap.delete (fst maxPayerTuple) (HashMap.delete (fst maxReciverTuple) balanceMap)
    where
        condition1 mp mr = (snd mp) > (abs (snd mr))
        condition2 mp mr = (snd mp) < (abs (snd mr)) 

    -- def getSuggestionsGreedy:
        -- payer_list = get_payer_list(balance_map)
        -- receive_list = get_receive_list(balance_map)
        -- suggestions = []

        -- while payer_list:
        --     max_payer = payer_list.pop((payer_list.index(max(payer_list))))
        --     max_receiver = receive_list.pop((receive_list.index(min(receive_list))))

        --     if max_payer[1] > -max_receiver[1]:
        --         max_payer[1] = max_payer[1] - (-max_receiver[1])
        --         payer_list.append(max_payer)
        --     else if max_payer[1] < -max_receiver[1]:
        --         max_receiver[1] = max_receiver[1] + max_payer[1]
        --         receive_list.append(max_receiver)

        --     suggestions.append([payer[0], receiver[0], min(max_payer[1], -max_receiver[1])])
        -- return suggestions


--  ******************************************* exact algorithm O(2^n * n^2) *******************************************

-- getSuggestionsDFS :: [MD.ExpenseRecord] -> [MD.SplitSuggestion]

--     def minTransfers(self, transactions: List[List[int]]) -> int:
--         balances = getTheBalanceMap(transactions)

--         if not balances:
--             return 0
        
--         to_update = {'max_num_subgroups': 1}
        
--         subgroups = [balances]
        
--         def explore_subgroups():
--             remaining_balances = subgroups.pop()

--             if remaining_balances:

--                 for subgroup_size in range(2, len(remaining_balances) + 1):
--                     for subgroup in itertools.combinations(remaining_balances, subgroup_size):
--                         if sum(remaining_balances[person] for person in subgroup) == 0:

--                             subgroup_balances = {}
--                             for person in subgroup:
--                                 subgroup_balances[person] = remaining_balances.pop(person)
--                             subgroups.append(subgroup_balances)
--                             subgroups.append(remaining_balances)
                            
--                             explore_subgroups()

--                             remaining_balances = subgroups.pop()
--                             subgroup_balances = subgroups.pop()
--                             for person in subgroup_balances:
--                                 remaining_balances[person] = subgroup_balances[person]
--             else:
--                 to_update['max_num_subgroups'] = max(
--                     to_update['max_num_subgroups'],
--                     len(subgroups))
            
--             subgroups.append(remaining_balances)
        
--         explore_subgroups()
--         return len(balances) - to_update['max_num_subgroups']


-- def minTransfers(self, transactions):
--     counter = collections.Counter()
--     for f, t, m in transactions:
--         counter[f] -= m
--         counter[t] += m
--     balances = counter.values()
--     def dfs(b):
--         if not b:
--             return 0
--         if not b[0]:
--             return dfs(b[1:])
--         for i in range(1, len(b)):
--             if b[i] == -b[0]:
--                 return 1 + dfs(b[1:i] + [0] + b[i+1:])
--         ret = []
--         for i in range(1, len(b)):
--             if b[i] * b[0] < 0:
--                 ret.append(dfs(b[1:i] + [b[i]+b[0]] + b[i+1:]))
--         return 1+min(ret)
    
--     return dfs(balances)


-- reference

-- https://www.alexirpan.com/2016/05/10/may-10.html
-- https://medium.com/@mithunmk93/algorithm-behind-splitwises-debt-simplification-feature-8ac485e97688
-- https://leetcode.com/problems/optimal-account-balancing/discuss/1299378/Insights-intuition-solution-complexity-analysis-(Python)