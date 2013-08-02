WordGames
=========

Common Lisp solver of some common word games. (Because my wife is
great at boggle, and I am most certainly not.)

Currently contains a boggle solver, with plans to add a scrabble
anagram solver and maybe some other stuff.

## words package

Contains functions that are common to many word games.

### build-dicts

    (build-dicts path)
    
Must be run before `word-p` and `prefix-p` can be called. Builds hash
tables of words using a given word list. The Official Scrabble
Player's Dictionary word list can be found in this repository. Other
word lists can be found [here](http://zyzzyva.net/wordlists.shtml).
Note that this can take significant amounts of time (~3 seconds on my
machine), so you may want to save a lisp image after you have the
dictionary built. 

If you don't want to use the included word list, please note that each
line of the file is being parsed using the following regex: `^([A-Z]{2,15})`.

### word-p

    (word-p string)
    
Predicate that tests whether or not `string` is a word.

### prefix-p 

    (prefix-p string)
    
Predicate that tests whether or not `string` is a prefix of another
word. Note that in this implementation, a word is not a prefix of
itself.    

### get-letter

    (get-letter)
    
Returns a random character according to it's frequency of use in
English according to
[this](http://en.wikipedia.org/wiki/Letter_frequency#Relative_frequencies_of_letters_in_the_English_language)
table.

Can be used to generate boggle boards.

## boggle package

### board

    (make-instance 'board &rest initargs &key generator data (size 4) &allow-other-keys)
    
The `size` slot is the size of the board (defaults to 4). 

To fill the `data` slot, you can either use the `:data` initarg, in
which case its length must be `(* size size)`:

    (make-instance 'board :size 3 :data "foobars")
    
or you can use the `:generator` key and supply a function which takes
the row and column of the current position as arguments, and returns a
character:

    (make-instance 'boggle:board
                   :generator (lambda (i j)
                                (declare (ignore i j))
                                (words:get-letter)))
    
### board-print

    (board-print board)
    
Helper function that pretty-prints a board.

    CL-USER> (boggle:board-print
              (make-instance 'boggle:board 
                             :size 3 
                             :data "foobarbaz"))
    f  o  o  
    b  a  r  
    b  a  z  
    NIL
    
### play-boggle

    (play-boggle board)
    
This does exactly what you'd expect.

    CL-USER> (boggle:play-boggle
              (make-instance 'boggle:board
                             :size 3
                             :data "foobarbaz"))
    f  o  o  
    b  a  r  
    b  a  z  
    
    baboo      bazar      fora       faro       boor      
    boar       bora       roof       baba       abba      
    fob        for        far        fab        oba       
    oaf        oar        ora        boo        boa       
    rob        baa        bar        aba        fa        
    of         or         bo         ba         aa        
    ar         ab         za        
    NIL
    
## anagram package

### solve

    (solve string)
    
TODO
    
## License

MIT
