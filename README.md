LISP
====

Bunch of projects I did during my Artificial Intelligence Class

There are 4 projects in this repository.

1. Convert to CNF - This lisp project converts any given logical sentence to CNF (Conjunctive Normal Form).
    
                    The input to the file is of the form ,
                    Input - (convert-to-CNF '(IF (AND A B C) (OR D E F G))) 
                    
                    Output will be in CNF format ,
                    Output - (OR (NOT A) (NOT B) (NOT C) D E F G)
                    
                    Input - (convert-to-CNF '(IF (NOT (OR A B)) C))
                    Output - (OR A B C)
                    
                    Input - (convert-to-CNF '(IFF A (OR B C)))
                    Output - (AND (OR (NOT A) B C) ((OR (NOT B) A) (OR (NOT C) A)))
          
          
2. Find Best Change - Given an amount and a set of coins, the program returns the best change (minimum number of coins).

                    Input - (find-best-change 30 '( 25 10 1))
                    Output - ((0 25) (3 10) (0 1))
                    
              
3. Place Knights on a matrix - Given a n x m matrix and a set of positions on which a knight cannot be placed, the program
        returs the list of positions on which the maximum number of knights can be placed and no two knights attack each
        other.
        
                    Input - (place-knights '( (4 5)))
                    Output - ((1 1) (1 3) (1 5) (2 2) (2 4) (3 1) (3 3) (3 5) (4 2) (4 4))
