module SplitAlgorithm where

import qualified Model.Data as MD
import qualified View.State as VS


-- get the balance map
-- getTheBalanceMap :: 
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

-- heuristic algorithm (O(n))

-- getPayerList :: 

-- getReceiverList :: 

-- getSuggestionsGreedy :: [MD.ExpenseRecord] -> [MD.SplitSuggestion]
-- getSuggestionsGreedy [] = []
-- getSuggestionsGreedy rs = 


--  exact algorithm O(2^n * n^2)

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


-- reference

-- https://www.alexirpan.com/2016/05/10/may-10.html
-- https://medium.com/@mithunmk93/algorithm-behind-splitwises-debt-simplification-feature-8ac485e97688
-- https://leetcode.com/problems/optimal-account-balancing/discuss/1299378/Insights-intuition-solution-complexity-analysis-(Python)