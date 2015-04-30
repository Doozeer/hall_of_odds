#lang racket

(require srfi/1)

;; Room descriptions list. Each sublist has the number of a room as
;; the first element, followed by the description of the room in
;; multiple strings
(define description-list
  '((1
     "You are in a small room, illuminated only by the light "
     "that shines in from two small windows in the walls to "
     "the south and west. You see a <<button>> on the north wall, "
     "near the northeast corner of the room.")
    (2
     "You are in a small room, light shines in through a single "
     "window on the south wall. You can see a <<button>> in the "
     "middle of the north wall.")
    (3
     "You are in a small room, illuminated only by the light "
     "that shines in from two small windows in the walls to "
     "the south and east. You see a <<button>> on the north wall, "
     "near the northwest corner of the room.")
    (4
     "You are in a small room, light shines in through a single "
     "window on the east wall. You can see a <<button>> in the "
     "middle of the west wall.")
    (5
     "You are in a small room, light shines in through three windows "
     "on the walls to the north, east, and south. You can see a "
     "<<button>> below the window in the north wall.")
    (6
     "You are in a small room, light shines in through a single "
     "window on the north wall. You can see a <<button>> below "
     "the window.")
    (7
     "You are in a small room. This one is poorly lit, with light "
     "coming in only from the neighbouring rooms, through the passages. "
     "Despite the lower visibility, you can see a <<button>> on the "
     "north wall, near the northeast corner of the room.")
    (8
     "You are in a small room, light shines in through a single "
     "window on the north wall. You can see a <<button>> below "
     "the window.")
    (9
     "You are in a small room, light shines in through three windows "
     "on the walls to the north, west, and south. You can see a "
     "<<button>> below the window in the north wall.")
    (10
     "You are in a small room, light shines in through a single "
     "window on the west wall. You can see a <<button>> in the "
     "middle of the east wall.")
    (11
     "You are in a small room, light shines in through two windows "
     "on the east and west walls. On the north wall, you see a big "
     "<<steel door>>, which is firmly shut. To the right of this door, "
     "you see a <<button>> on the wall.")
    (12
     "Congratulations! You have escaped the Hall of Odds!")))

;; Room connections mapping. Each sublist begins with the number of
;; a room, followed by lists associating each direction to the number
;; of the connecting room. A zero means there's no room connected in
;; that direction.
(define directions-list
  '((1  (north 10) (south 0)  (east 2) (west 0))
    (2  (north 0)  (south 0)  (east 3) (west 1))
    (3  (north 4)  (south 0)  (east 0) (west 2))
    (4  (north 6)  (south 3)  (east 0) (west 0))
    (5  (north 0)  (south 0)  (east 0) (west 6))
    (6  (north 0)  (south 4)  (east 5) (west 7))
    (7  (north 11) (south 0)  (east 6) (west 8))
    (8  (north 0)  (south 10) (east 7) (west 9))
    (9  (north 0)  (south 0)  (east 8) (west 0))
    (10 (north 8)  (south 1)  (east 0) (west 0))
    (11 (north 0)  (south 7)  (east 0) (west 0))
    (12 (north 0)  (south 0)  (east 0) (west 0))))

;; Objects available for 'push' in each room. Each sublist begins
;; with the number of a room, followed by lists containing the names
;; of the objects present in it.
(define pushables-list
  '((1  (button))
    (2  (button))
    (3  (button))
    (4  (button))
    (5  (button))
    (6  (button))
    (7  (button))
    (8  (button))
    (9  (button))
    (10 (button))
    (11 (button) (steel door))
    (12 )))

;; Common error messages
(define interpreter-fail "Sorry, I couldn't understand what you want to do.\n")
(define unknown-error "[ERROR] Sorry, something strange happened!\n")

;; Hash-table declarations
(define commands      (make-hash))
(define descriptions  (make-hash))
(define directions    (make-hash))
(define pushables     (make-hash))
(define obj-actions   (make-hash))
(define button-states (make-hash))

;; Initializes variables before starting the game
(define (initialize)
  (set-game-commands command-list)
  (set-room-descriptions description-list)
  (set-room-directions directions-list)
  (set-room-pushables pushables-list)
  
  ;; Mapping functions to objects
  (hash-set! obj-actions '(button) button)
  (hash-set! obj-actions '(steel door) door)
  
  ;; Init deactivated buttons
  (for-each (lambda (n)
              (hash-set! button-states n #f))
            (hash-keys descriptions)))

;; Function to set data from an associative list into the room description
;; hash-table.
(define (set-room-descriptions description-list)
  (for-each (lambda (description)
              (hash-set! descriptions
                         (car description)
                         (string-append* (cdr description))))
            description-list))

;; Function to set data from an associative list into the room directions
;; hash-table.
(define (set-room-directions direction-list)
  (for-each (lambda (direction)
              (hash-set! directions (car direction) (cdr direction)))
            direction-list))

;; Function to set data from an associative list into the room pushables
;; hash-table.
(define (set-room-pushables pushables-list)
  (for-each (lambda (pushable)
              (hash-set! pushables (car pushable) (cdr pushable)))
            pushables-list))

;; Function to set data from an associative list into the game commands
;; hash-table.
(define (set-game-commands command-list)
  (for-each (lambda (command-mapping)
              (let [(command-tokens (car command-mapping))
                    (function (car (cdr command-mapping)))]
                (for-each (lambda (command-token)
                            (hash-set! commands command-token function))
                          command-tokens)))
            command-list))

;; Function to set data from an associative list into the object actions
;; hash-table.
(define (set-obj-actions obj-actions-list)
  (for-each (lambda (action-mapping)
              (let [(objects (car action-mapping))
                    (function (car (cdr action-mapping)))]
                (for-each (lambda (object)
                            (hash-set! obj-actions object function))
                          objects)))
            obj-actions-list))

;; Function that exits the game. args may be empty or have only the word 'game'
;; so the command makes sense, i.e. 'quit' and 'quit game' are both valid
(define (quit args rid)
  (if (or (null? args) (and (eq? (car args) 'game) (null? (cdr args))))
      (exit)
      ((printf interpreter-fail)
       (new-cycle rid))))

;; The same as the function quit, but for the 'exit game' command, and will
;; only accept empty args. This way, typing simply 'exit', which would be
;; too much ambiguous, won't quit the game.
(define (exitgame args rid)
  (if (null? args)
      (exit)
      ((printf interpreter-fail)
       (new-cycle rid))))

;; Process move command, in which args must specify a valid direction in which
;; to move from the specified room
(define (move args rid) 
  (cond
    ((or (null? args) (> (length args) 1))
     (printf "Sorry, please say one valid direction to move (north, south, east, west).\n")
     (new-cycle rid))
    ((member (car args) '(north south east west))
     (let ((new-room (lookup rid (car args))))
       (if (zero? new-room)
           ((printf "Sorry, you can't go ~a from your current room.\n" (car args))
            (new-cycle rid))
           ((printf "Moving to the next room to the ~a...\n" (car args))
            (new-cycle new-room)))))
    (else 
     (printf "Sorry, '~a' isn't a valid direction to move.\n" (car args))
     (new-cycle rid))))

;; Process the push command, in which args must be a valid item in the
;; pushables hash-table for the specified room
(define (push args rid)
  (if (member args (hash-ref pushables rid))
      ((hash-ref obj-actions args) rid)
      ((printf "Please specify a valid object to push!\n")
       (new-cycle rid))))

;; Execute button action in the specified room
(define (button rid)
  (case (hash-has-key? button-states rid)
    ((#t)
     (hash-set! button-states rid (not (hash-ref button-states rid)))
     (printf "You push the button. A high pitched beep sounds, but you wonder what it means.\n")
     (eval-buttons))
    ((#f) (printf "[ERROR] Button undefined for room ~a.\n" rid)))
  (new-cycle rid))

;; Control opening of the steel door. If the door is closed and all button
;; are active, the description, room connections, and  list of interactive
;; items in room 11 are changed to reflected its opening. If the door is
;; open and at least one button is inactive, the door is closed.
(define (eval-buttons)
  (if (zero? (lookup 11 'north))
      (case (every (lambda (x) (eq? x #t)) (hash-values button-states))
        ((#t)
         (hash-set! directions 11 '((north 12)  (south 7)  (east 0) (west 0)))
         (hash-set! descriptions 11 (string-append
                                     "You are in a small room, light shines in through two windows "
                                     "on the east and west walls. On the north wall, the big "
                                     "steel door is now open, allowing you to exit the halls. To "
                                     "the right of this door, you see a <<button>> on the wall."))
         (hash-set! pushables 11 '((button)))
         (printf "A moment later, you hear a loud sound, as if something has been unlocked.\n")))
      (case (every (lambda (x) (eq? x #t)) (hash-values button-states))
        ((#f)
         (hash-set! directions 11 '((north 0)  (south 7)  (east 0) (west 0)))
         (hash-set! descriptions 11 (string-append
                                     "You are in a small room, light shines in through two windows "
                                     "on the east and west walls. On the north wall, you see a big "
                                     "<<steel door>>, which is firmly shut. To the right of this door, "
                                     "you see a <<button>> on the wall."))
         (hash-set! pushables 11 '((button) (steel door)))
         (printf "A moment later, you hear a loud sound, as if a door has been shut.\n")))))

;; Execute steel door action
(define (door rid)
  (printf "You try to push the steel door, but it just won't budge...\n")
  (new-cycle rid))

;; Get a list of connected directions to move from a room
(define (get-available-dirs rid)
  (let ((room-dirs (get-room-dirs rid)))
    (if (pair? room-dirs)
        (map car (filter
                  (lambda (x)
                    (> (car (cdr x)) 0))
                  room-dirs))
        room-dirs)))

;; Print to the user a list of directions he can move from the current room
(define (print-available-dirs rid)
  (let ((dirs (get-available-dirs rid)))
    (if (list? dirs)
        (case (length dirs)
          ((0) (printf "You see no exits from this room!\n"))
          ((1) (printf "You see an exit to the ~a.\n" (car dirs)))
          (else (printf "You see exits to the ~a~a" (car dirs) (append-dirs (cdr dirs)))))
        (printf "[ERROR] Could not retrieve directions for room ~a!\n" rid))))

;; Auxiliary function to build strings for the print-available-dirs function
(define (append-dirs dirs)
  (if (null? (cdr dirs))
      (format " and ~a.\n" (car dirs))
      (format ", ~a~a" (car dirs) (append-dirs (cdr dirs)))))

(define (assq-ref assqlist id)
  (let ((list (assq id assqlist)))
    (if (pair? list)
        (cdr list)
        list)))

;; Gets the description for a room
(define (get-room-description rid)
  (let ((description (hash-ref descriptions rid #f)))
    (if (string? description)
        description
        (format "[ERROR] Could not retrieve description for room ~a!\n" rid))))

;; Gets the directions mapping for a room
(define (get-room-dirs rid)
  (hash-ref directions rid #f))

;; Returns the ID of the room that lies at the specified direction from
;; the room identified by 'room-id'
(define (lookup room-id direction)
  (car (assq-ref (get-room-dirs room-id) direction)))

;; Command interpreting function. It tries to match as little as possible
;; of the first words in the command to one of the initial keywords specified
;; in the commands hash-table. It begins trying to match the first word to a
;; command and use the remaining words as arguments for that command. When no
;; match is found, the first word of the arguments is moved to the tail of the
;; command, and matching it attempted again. This process is repeated until a
;; either a match is found and the assigned function is executed, or the
;; argument list is empty and the user is alerted that his command was invalid
;; and the game repeats one cycle.
(define (interpret command rid)
  (if (not (null? command))
      (let try [(action `(,(car command)))
                (args (cdr command))]
        (let ((procedure (hash-ref commands action #f)))
          (cond
            ((procedure? procedure) (procedure args rid))
            ((eq? procedure #f) (if (null? args)
                                    ((printf interpreter-fail)
                                     (new-cycle rid))
                                    (try `(,@action ,(car args)) (cdr args))))
            (else (printf unknown-error)))))
      ((printf interpreter-fail)
       (new-cycle rid))))

;; Reads a line from the user and converts it to a list of symbols
(define (read-command)
  (map string->symbol (string-split (read-line))))

;; This function executes a cycle of the game, i.e. describe room,
;; check for game termination conditions, and ask command
(define (new-cycle rid)
  (printf "~a\n" (get-room-description rid))
  (if (eq? rid 12) (exit) 'continue)
  (print-available-dirs rid)
  (printf "> ")
  (interpret (read-command) rid))

;; This function begins the game
(define (startgame room-id)
  (initialize)
  (printf (string-append
           "Trapped within the rooms of the Hall of Odds, are you "
           "wise enough to escape its walls, or are you fated to perish "
           "in this place without ever knowing freedom again?\n"))
  (new-cycle room-id))

;; Game's main commands. Each sublist begins with a list of commands separated
;; in lists, followed by a reference to the function that is related to those
;; commands.
(define command-list
  `((((move) (go) (walk)) ,move)
    (((quit)) ,quit)
    (((exit game)) ,exitgame)
    (((push)) ,push)))

;; Mapping game objects to action functions. Each sublist begins with a list of
;; objects separated in lists, followed by a reference to the function that is
;; related to those objects.
(define obj-action-list
  `((((button)) ,button)
    (((steel door)) ,door)))

(startgame 1)