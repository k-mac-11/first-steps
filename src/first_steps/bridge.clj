;start torch is 1 and state is [1, 1, 1, 1] and time is [t1, t2, t3, t4]
;goal is torch is 0 and state is [0, 0, 0, 0]
;while not (torch = 0 and state = [0, 0, 0, 0])
;if torch is 1 then torch goes to 0 and generate all possible two positions of state go from 1 to 0. say position i and j. time taken is the max of ti and tj.
;if torch is 0 then torch goes to 1 and one position of state goes from 0 to 1. say position i. time taken is ti.

time taken is max of the ti's

(defn next-move (state)
  (if (> torch 0)
    ))
