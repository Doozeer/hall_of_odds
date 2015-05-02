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

;; Alternate description for the room with the door
(define alt-description 
  (string-append "You are in a small room, light shines in through two windows "
                 "on the east and west walls. On the north wall, the big "
                 "steel door is now open, allowing you to exit the halls. To "
                 "the right of this door, you see a <<button>> on the wall."))

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

;; Alternate directions for the room with the door
(define alt-directions
  '((north 12)  (south 7)  (east 0) (west 0)))

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

;; Alternate pushables for the room with the door
(define alt-pushables
  '((button)))

;; Game's main commands. Each sublist begins with a list of commands separated
;; in lists, followed by a reference to the function that is related to those
;; commands. This is defined as a function so it can be defined before the
;; definition of the functions.
(define (command-list)
  `((((move) (go) (walk)) ,move)
    (((quit) (quit game) (exit game)) ,quit)
    (((push)) ,push)))

;; Mapping game objects to action functions. Each sublist begins with a list of
;; objects separated in lists, followed by a reference to the function that is
;; related to those objects. This is defined as a function so it can be defined
;; before the definition of the functions.
(define (obj-action-list)
  `((((button)) ,button)
    (((steel door)) ,door)))

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
(define alt-states    (make-hash))

;; Initializes variables before starting the game
(define (initialize)
  (set-game-commands (command-list))
  (set-room-descriptions description-list)
  (set-room-directions directions-list)
  (set-room-pushables pushables-list)
  (set-obj-actions (obj-action-list))
  
  ;; Init alt-states
  (hash-set! alt-states 'description alt-description)
  (hash-set! alt-states 'directions  alt-directions)
  (hash-set! alt-states 'pushables   alt-pushables)
  
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

;; This function handles the commands to quit the game.
(define (quit args rid)
  (if (null? args)
      (exit)
      ((printf interpreter-fail)
       (new-cycle rid))))

;; Process move command, in which args must specify a valid direction in which
;; to move from the specified room
(define (move args rid)
  (if (or (null? args) (> (length args) 1))
      ((printf "Sorry, please say one valid direction to move (north, south, east, west).\n")
       (new-cycle rid))
      (let [(direction (car args))]
        (if (member direction '(north south east west))
            (let [(new-room (lookup rid direction))]
              (if (zero? new-room)
                  ((printf "Sorry, you can't go ~a from your current room.\n" direction)
                   (new-cycle rid))
                  ((printf "Moving to the next room to the ~a...\n" direction)
                   (new-cycle new-room))))
            ((printf "Sorry, '~a' isn't a valid direction to move.\n" direction)
             (new-cycle rid))))))

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
         (swap-states 11)
         (printf "A moment later, you hear a loud sound, as if something has been unlocked.\n")))
      (case (every (lambda (x) (eq? x #t)) (hash-values button-states))
        ((#f)
         (swap-states 11)
         (printf "A moment later, you hear a loud sound, as if a door has been shut.\n")))))

;; This function swaps room states to open and close the door.
(define (swap-states rid)
  (let [(temp-descr (hash-ref alt-states 'description))
        (temp-dirs  (hash-ref alt-states 'directions))
        (temp-pushs (hash-ref alt-states 'pushables))]
    ;; Swap descriptions
    (hash-set! alt-states 'description (hash-ref descriptions rid))
    (hash-set! descriptions rid temp-descr)
    ;; Swap directions
    (hash-set! alt-states 'directions (hash-ref directions rid))
    (hash-set! directions rid temp-dirs)
    ;; Swap pushables
    (hash-set! alt-states 'pushables (hash-ref pushables rid))
    (hash-set! pushables rid temp-pushs)))

;; Execute steel door action
(define (door rid)
  (printf "You try to push the steel door, but it just won't budge...\n")
  (new-cycle rid))

;; Get a list of connected directions to move from a room
(define (get-available-dirs rid)
  (let ((room-dirs (get-room-dirs rid)))
    (if (list? room-dirs)
        (map car
             (filter
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

;; Command interpreting function. It tries to separate the input into
;; a known command and its arguments, and then executes it.
(define (interpret command rid)
  (if (not (null? command))
      (let* [(split-command (split-longest-match command))]
        (if split-command
            (exec-split-command split-command rid)
            (show-interpreter-fail rid)))
      (show-interpreter-fail rid)))
        
;; Splits the command into a list containing the longest key from the
;; commands hash-table found in the command, followed by the arguments.
(define (split-longest-match command)
  (let loop [(action `(,(car command)))
             (args (cdr command))
             (longest-match #f)]
    (if (null? args)
        (if (hash-has-key? commands action)
            (build-split-command-list action args)
            longest-match)
        (if (hash-has-key? commands action)
            (loop `(,@action (car args)) (cdr args) (build-split-command-list action args))
            (loop `(,@action (car args)) (cdr args) longest-match)))))

;; This function builds a list separating the main command in a sublist and
;; the arguments following that.
(define (build-split-command-list command args)
  `(,command ,@args))

;; Attempts to execute a list with previously separated command and arguments.
(define (exec-split-command split-command rid)
  (let [(function (hash-ref commands (car split-command) #f))
        (args (cdr split-command))]
    (if (procedure? function)
        (function args rid)
        (show-interpreter-fail rid))))

;; Shows interpreter error message and begins new cycle
(define (show-interpreter-fail rid)
  (printf interpreter-fail)
  (new-cycle rid))

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

(startgame 1)