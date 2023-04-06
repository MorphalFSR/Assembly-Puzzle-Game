; multi-segment executable file template.

data segment
    
    ; character position (high byte = y, low byte = x)
    currentpos dw 0 
    pastpos dw 0
    
    ; tuples of teleporter positions
    redtelepos dw 2 dup(0)
    yellowtelepos dw 2 dup(0)
    bluetelepos dw 2 dup(0)
    
    ; enemy positions (x,y) and offsets (dx,dy)
    ; first enemy stays 0 for technicality reasons
    enemypos dw 10 dup(0)
    enemyoffs dw 10 dup(0)
    nextenemy db 1
    
    ; for advanced enemy-player interaction checks
    intersectionflag db 0
    
    ; map indexes and other map related useful variables
    playerindex dw ?
    keypos dw ?
    filehandle dw ?
    filename db "level00.txt", 0
    currentlevel dw "00"
    mapsize dw 14
    rowlengths db 41 dup(0)
    map db "Keys: X X X", 0dh, 0ah, 41*25 dup(' ')
    
    ; special tile characters
    Wall EQU 0xb0h
    Key EQU 0ch
    Door EQU 15h
    Teleporter EQU 0xe9h 
    Flag EQU 0eh          
    FlagColor EQU 000ah
    Rock EQU 0fh
    Enemy EQU 0eah
    
    ; useful constants
    LevelIDTens EQU filename[5] 
    LevelIDUnits EQU filename[6]
    RedKey EQU 6
    YellowKey EQU 8
    BlueKey EQU 10
    GameOverID EQU '{'
    GameFinishID EQU '}'
    
ends

stack segment
    dw   128  dup(0)
ends

code segment
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax

    ; add your code here
    
    jmp next_level_normally
    
game_over:
    ; set level to Game Over level
    mov LevelIDTens, GameOverID
    mov LevelIDUnits, GameOverID
    jmp start_level

next_level:
    
    ; check if game is finished
    cmp LevelIDUnits, GameFinishID + 1
    je terminate
    
    ; check if level is the game over screen, and if so return to the correct level
    cmp LevelIDUnits, GameOverID + 1
    jne next_level_normally
    mov ax, currentlevel
    mov LevelIDUnits, ah
    mov LevelIDTens, al
    jmp next_level_normally
    
next_level_normally:
    mov ah, LevelIDUnits 
    mov al, LevelIDTens  
    mov currentlevel, ax
    
    ; check if next level exists
    
    ; open file (to check if it exists)
    mov ah, 3dh
    mov al, 0
    lea dx, filename
    int 21h
    
    ; check if ax is [file not found] error code
    cmp ax, 02
    je finish_game
    
    ; close file
    mov ah, 3eh
    int 21h

start_level:  
    
    ; reset level specific variables
    ; reset teleporter locations 
    mov bluetelepos[0], 0
    mov bluetelepos[1], 0
    mov yellowtelepos[0], 0
    mov yellowtelepos[1], 0
    mov redtelepos[0], 0
    mov redtelepos[1], 0 
    
    ; reset row lengths
    mov bx, 0
    mov cx, 41
reset_row_lengths:
    mov rowlengths[bx], 0
    inc bx
    loop reset_row_lengths
    
    ; reset map array
    mov cx, mapsize
    sub cx, 13
    mov bx, 13
reset_map_array:
    mov map[bx], ' '
    inc bx
    loop reset_map_array
    
    ; reset map size
    mov mapsize, 14
    
    ; reset keys
    mov map[RedKey], 'X'
    mov map[YellowKey], 'X'
    mov map[BlueKey], 'X'
    
    ; reset enemy info
    mov nextenemy, 1
    
    mov bx, 0
    mov cx, 10
reset_enemy_info:
    mov enemypos[bx], 0
    mov enemyoffs[bx], 0
    inc bx
    loop reset_enemy_info
    
    ; load map from file
    call loadlevel
    
    ; set graphics mode
    mov ah, 0
    mov al, 1
    int 10h 
    
    ; print map from file
    call printmap

new_cycle:
     
    ; write player character at current position
    push 2
    push 000eh
    mov dx, currentpos
    push dx
    call writechar
    
    ; check if key pressed
    mov ah, 1
    int 16h
    jz dont_move
    mov ah, 0
    int 16h
    
    ; set past position to current position
    mov dx, currentpos
    mov pastpos, dx
    
    ; set coordinates to new position
    cmp al, 'w'
    jne not_up
    dec dh
    jmp moved
    
not_up:
    cmp al, 'a'
    jne not_left
    dec dl
    jmp moved

not_left:    
    cmp al, 's'
    jne not_down
    inc dh
    jmp moved

not_down:
    cmp al, 'd'
    jne not_right   
    inc dl
    jmp moved
    
    ; reload level by pressing r
not_right:
    cmp al, 'r'
    je start_level
    
    jmp new_cycle
    

moved:    

    ; check if the player can move to the new position
    mov ah, LevelIDUnits ; save to check if the level stays constant
    push dx
    call checkmove
    jz stay_put
    cmp ah, LevelIDUnits ; if the level ID changes (only units need to be checked)
    jne next_level ; then start the next level
    
    ; delete character at current position
    push dx ; save dx for new position
    mov dx, currentpos
    mov bx, playerindex
    push bx
    push dx
    call writemaptile
    ; get past position back to dx
    pop dx

    ; write player character at new position
    mov currentpos, dx
    push 2
    push 000eh
    mov dx, currentpos
    push dx
    call writechar
    
    ; set new player index
    push dx
    call findmapindex
    mov playerindex, bx
    
    ; key pressed, but player stays at current position (can't move)
stay_put:
    
    ; check if there are any enemeies at all
    cmp enemyoffs[2], 0
    je dont_move ; also functions as end of enemy procedures line
    
    ; move enemies
    mov bx, 2 
move_next_enemy:
    mov ax, enemypos[bx]
    
    ; check if enemy past position is player new position
    cmp ax, currentpos
    jne not_intersecting
    mov intersectionflag, 1
not_intersecting:
    
    add ax, enemyoffs[bx]
    push ax
    call checkenemymove
    jz switch_direction ; if the enemy cannot move, switch its direction
    
    ; delete enemy from past position
    mov ax, 0
    push ax
    mov ax, enemypos[bx]
    push ax
    call writemaptile
    
    mov enemypos[bx], dx
    
    ; print the enemy
    mov al, Enemy
    push ax
    mov ax, 0006h
    push ax
    mov ax, enemypos[bx]
    push ax
    call writechar
    
    jmp finish_moving_enemy
    
switch_direction:
    mov ax, 0
    mov dx, enemyoffs[bx]
    sub ax, dx
    mov enemyoffs[bx], ax
    
    ; move enemy one step back
    mov ax, enemypos[bx]
    add ax, enemyoffs[bx]
    push ax
    call checkenemymove
    jz finish_moving_enemy ; if the enemy cannot move, pass to the next one
    
    ; delete enemy from past position
    mov ax, 0
    push ax
    mov ax, enemypos[bx]
    push ax
    call writemaptile
    
    mov enemypos[bx], dx
    
    ; print the enemy
    mov al, Enemy
    push ax
    mov ax, 0006h
    push ax
    mov ax, enemypos[bx]
    push ax
    call writechar
    
    
finish_moving_enemy:
    
    ; game over if player and enemy overlap   
    mov ax, enemypos[bx]
    mov dx, currentpos
    cmp ax, dx
    je game_over
    
    ; game over if player and enemy intersected
    mov ax, enemypos[bx]
    cmp ax, pastpos
    jne dont_reload
    cmp intersectionflag, 1
    jne dont_reload
    jmp game_over

dont_reload:
    
    ; move to next enemy
    inc bx
    inc bx
    cmp enemyoffs[bx], 0
    jne move_next_enemy 
    
    ; end of enemy procedures
    
dont_move:
    
    ; finish this cycle's procedures
    jmp new_cycle

finish_game:
    
    ; set level id to game finish level id
    mov LevelIDUnits, GameFinishID    
    mov LevelIDTens, GameFinishID
    jmp next_level

terminate:

    ; set graphics mode (clear screen)
    mov ah, 0
    mov al, 1
    int 10h
    
    mov ax, 4c00h ; exit to operating system.
    int 21h
           
ends

CHAR EQU bp+8
COLOR EQU bp+6 ; top byte is zero, lowest byte is char color, the one above is background color
POS EQU bp+4 ; top byte is row number, lower byte is col number   
proc writechar
    push bp
    mov bp, sp
    pusha
    
    ; move cursor position to x,y
    mov ah, 2
    mov bh, 0
    mov dx, [POS]
    int 10h
    
    ; write character at position
    mov ax, [CHAR]
    mov ah, 9
    mov bx, [COLOR]
    mov cx, 1
    int 10h
    
    popa
    pop bp
    ret 6
endp writechar


proc loadlevel
    
    pusha
    
    ; open file
    mov ah, 3dh
    mov al, 0
    lea dx, filename
    int 21h
    mov bx, ax
    
    ; load chars from file into map
    mov ah, 3fh
    mov cx, 41*25
    lea dx, map
    add dx, 13
    int 21h
    mov mapsize, 13
    add mapsize, ax
    
    ; close file
    mov ah, 3eh
    int 21h
    
    popa
    ret
endp loadlevel


; prints the map and processes special tiles (player, keys, doors, teleporters, enemies)
proc printmap
    
    pusha
    
    xor ax, ax
    xor bx, bx
    xor dx, dx
    ; bx is the current char's index within the loaded map 
    ; dx is used to represent current char's position on the screen
    ; dh is row number, dl is col number
    mov cx, mapsize
    
    ; write next char from loaded map on the screen
write_tile:
    mov al, map[bx]
    
    ; single-byte tiles
    cmp al, 0dh ; check if passing to a new line is needed (compare to carriage return)
    je move_line
    cmp al, 'p'
    je set_player
    cmp al, 'w'
    je set_wall
    cmp al, 'f'
    je print_flag
    cmp al, 'r'
    je print_rock
    
    ; add one to current row's length (tile is double-byte or not special. if not special, length will be fixed)
    push bx
    xor bx, bx
    mov bl, dh
    inc rowlengths[bx]
    pop bx
    
    cmp dh, 0
    je not_special
    
    ; double-byte tiles (tile character + color/direction)
    cmp al, 'k'
    je print_key
    cmp al, 'd'
    je print_door
    cmp al, 't'
    je print_teleporter
    cmp al, 'e'
    je print_enemy
not_special:
    push ax
    mov ax, 000fh 
    push ax
    push dx
    call writechar
    
    ; subtract one to current row's length (cancel double-bye assumption)
    push bx
    xor bx, bx
    mov bl, dh
    dec rowlengths[bx]
    pop bx
    
    jmp dont_move_line
    
    ;print key with color
print_key:
    mov map[bx], Key
    mov al, Key
    push ax
    mov al, map[bx+1]
    push ax
    call getcolor
    push ax
    push dx
    call writechar
    inc bx
    
    jmp dont_move_line
    
    ; print door with color
print_door:
    mov map[bx], Door
    mov al, Door
    push ax
    mov al, map[bx+1]
    push ax
    call getcolor
    push ax
    push dx
    call writechar
    inc bx
    
    jmp dont_move_line
    
    ; print teleporter and set position in matching tuple
print_teleporter:
    mov map[bx], Teleporter
    mov al, Teleporter
    push ax
    mov al, map[bx+1]
    push ax
    call getcolor
    push ax
    push dx
    call writechar
    inc bx
    
    mov al, map[bx]
    cmp al, 'r'
    je red_teleporter
    cmp al, 'y'
    je yellow_teleporter
    
    mov ax, bluetelepos[0]
    cmp ax, 0
    jne second_blue
    mov bluetelepos[0], dx
    jmp dont_move_line
second_blue:
    mov bluetelepos[2], dx
    jmp dont_move_line
    
red_teleporter:
    mov ax, redtelepos[0]
    cmp ax, 0
    jne second_red
    mov redtelepos[0], dx
    jmp dont_move_line
second_red:
    mov redtelepos[2], dx
    jmp dont_move_line
    
yellow_teleporter:
    mov ax, yellowtelepos[0]
    cmp ax, 0
    jne second_yellow
    mov yellowtelepos[0], dx
    jmp dont_move_line
second_yellow:
    mov yellowtelepos[2], dx
    jmp dont_move_line
    
    ; set player position in map
set_player:
    mov currentpos, dx
    mov playerindex, bx
    
    mov map[bx], ' '
    mov ax, ' '
    push ax
    mov ax, 000fh 
    push ax
    push dx
    call writechar
    
    jmp dont_move_line

    ; set wall tile to wanted character in map
set_wall:
    mov map[bx], Wall
    mov ax, Wall
    push ax
    mov ax, 000fh 
    push ax
    push dx
    call writechar
    
    jmp dont_move_line
    
    
    ; the flag is the end point for the level loads next level when reached
print_flag:
    mov map[bx], Flag
    mov ax, Flag
    push ax
    mov ax, FlagColor
    push ax
    push dx
    call writechar
    
    jmp dont_move_line
    
    ; print rock tile
print_rock:
    mov map[bx], Rock
    mov ax, Rock
    push ax
    mov ax, 0007h
    push ax
    push dx
    call writechar
    
    jmp dont_move_line
    
    ; print the enemy and add it to the enemy array
print_enemy:
    mov map[bx], 's'
    mov al, map[bx+1]
    push ax
    call getenemydirection
    push bx
    xor bx, bx
    mov bl, nextenemy
    add bl, nextenemy
    mov enemypos[bx], dx
    mov enemyoffs[bx], ax
    inc nextenemy  
    pop bx
    
    mov ax, Enemy
    push ax
    mov ax, 0006h
    push ax
    push dx
    call writechar
    inc bx
    
    jmp dont_move_line
        

    ; pass to the next row/line
move_line:
    inc bx
    dec cx
    
    ; add one to current row's length
    push bx
    xor bx, bx
    mov bl, dh
    inc rowlengths[bx]
    pop bx
    
    
    inc dh
    mov dl, -1
    
    
    ; mov to next char in loaded map
dont_move_line:    
    inc bx
    
    ; add one to current row's length or previous row if current row is new (dl=-1)
    push bx
    xor bx, bx
    mov bl, dh
    cmp dl, -1
    jne this_row
    inc rowlengths[bx-1]
    jmp finish_tile
this_row:
    inc rowlengths[bx]
finish_tile:
    pop bx
    
    inc dl
    loop write_tile
    
    popa
    ret

endp printmap


; return into bx: index within loaded map that represents char at the wanted position on the screen
POS_TO_FIND EQU bp+4
proc findmapindex
    
    push bp
    mov bp, sp
    push ax
    push dx
    push cx
    
    xor ax, ax
    xor bx, bx
    xor cx, cx
    mov dx, [POS_TO_FIND]
    mov cl, dh
    xor dx, dx
    cmp cx, 0
    je check_row_cells
add_row_lengths:
    mov dl, rowlengths[bx]
    add ax, dx
    inc bx
    loop add_row_lengths
    
check_row_cells:
    mov bx, ax
    mov dx, [POS_TO_FIND]
    mov dl, 0
    mov cx, mapsize
    sub cx, bx
    
check_next_char:
    mov al, map[bx]
    cmp al, 0dh ; check if passing to a new line is needed (compare to carriage return)
    je next_line
    cmp dx, [POS_TO_FIND] ; checks if current char is at the wanted position
    je found_index
    cmp al, Key
    je special_character
    cmp al, 's'
    je special_character
    cmp al, Door
    je special_character
    cmp al, Teleporter
    je special_character
    jmp same_line

special_character:
    inc bx
    jmp same_line
    
    
next_line:
    mov dl, -1
    inc dh
    inc bx

; move to next char in loaded map
same_line: 
    inc dl
    inc bx
    loop check_next_char
    
    ; set ZF = 1 if position is not in map
    xor ax, ax
    test ax, ax

found_index:
    ; set ZF = 0 if position is in map
    xor ax, ax
    inc ax
    
    pop cx
    pop dx
    pop ax
    pop bp
    
    ret 2
    
endp findmapindex


; get color word value from char and load it into ax
COLOR_NAME EQU bp+4
proc getcolor
    
    push bp
    mov bp, sp
    
    mov ax, [COLOR_NAME]
    cmp al, 'r'
    je red
    cmp al, 'y'
    je yellow
    cmp al, 'b'
    je blue
    mov ax, 000fh
    jmp return_color

red:
    mov ax, 000ch
    jmp return_color
yellow:
    mov ax, 000eh
    jmp return_color
blue:
    mov ax, 0009h
    jmp return_color
    
return_color:
    pop bp
    ret 2
    
endp getcolor


; writes specific tile from the map
KNOWN_INDEX EQU bp+6 ; used for optimization, can be given the tiles index in the map if it is known.
; 0 to search, any other number within the map for a known index, mapsize or greater if the tile is outside the map
TILE_POS EQU bp+4 ; tile coordinates in map
proc writemaptile
    
    push bp
    mov bp,sp
    pusha
    
    mov bx, [KNOWN_INDEX]
    cmp bx, 0
    jne dont_search_index
    
    mov dx, [TILE_POS]
    push dx
    call findmapindex
    jz write_empty
    jmp process_tile

dont_search_index:
    cmp bx, mapsize
    jnb write_empty

process_tile:
    
    ; check if tile is some kind of special tile
    mov al, map[bx]
    cmp al, Key
    je write_key
    cmp al, Door
    je write_door
    cmp al, Teleporter
    je write_teleporter
    cmp al, Rock
    je write_rock
    
    
    ; check if tile is a skip tile
    cmp al, 's'
    je write_empty
    
    ; write normal tile from map
    push ax
    mov ax, 000fh
    push ax
    push dx
    call writechar
    jmp written

    ; write special tiles (keys, doors, teleporters) with color
write_teleporter:
    mov al, Teleporter
    jmp write_special_tile
write_key:
    mov al, Key
    jmp write_special_tile
write_door:
    mov al, Door
    jmp write_special_tile
write_special_tile:
    push ax
    mov al, map[bx+1]
    push ax
    call getcolor
    push ax
    push dx
    call writechar
    jmp written

write_rock:
    mov map[bx], Rock
    mov ax, Rock
    push ax
    mov ax, 0007h
    push ax
    push dx
    call writechar
    
    jmp written

; write space if tile is empty or a skip tile
write_empty:
    xor ax, ax
    mov al, ' '
    push ax
    mov ax, 0000h
    push ax
    push dx
    call writechar
    
written:
    
    popa
    pop bp
    
    ret 4

endp writemaptile


; check if destination is possible and change destination tile if necessary
; returns ZF = 1 if moving to destination is possible
; returns ZF = 0 if moving to destination is not possible
; returns to dx: new position
WANTED_POS EQU bp+4
proc checkmove
    
    push bp
    mov bp,sp
    push ax
    push bx
    
    ; get destination's map index
    mov dx, [WANTED_POS]
    push dx
    call findmapindex
    jz can_move ; move to destination if tile is not in the map (empty)
    
    ; check special tile requirements and conditions
    mov al, map[bx]
    cmp al, Wall ; dont move if destination is a wall
    je cant_move
    cmp al, Key ; get key if destination is a key
    je get_key
    cmp al, Door ; try to open door if destination is door
    je try_door
    cmp al, Teleporter ; teleport if destination is teleporter
    je teleport
    cmp al, Flag ; move to next level if teleporter is reached
    je finish_level
    cmp al, Rock
    je push_rock
    
    jmp can_move ; move to destionation if a tile doesn't require special conditions


    ; obtain key
get_key:
    push ax
    mov map[bx], 's' ; set skip tile in place of the key
    
    ; get key's color
    mov al, map[bx+1]
    push ax
    call getcolor
    push ax 
    
    ; set key's position in key inventory
    mov al, map[bx+1]
    cmp al, 'r'
    je get_red_key
    
    cmp al, 'y'
    je get_yellow_key
    
    mov ax, BlueKey
    push ax
    jmp add_key    
    
get_red_key:
    mov ax, RedKey
    push ax
    jmp add_key

get_yellow_key:
    mov ax, YellowKey
    push ax
    jmp add_key
    
    ; add key to key inventory
add_key:
    mov bx, ax
    mov map[bx], Key
    call writechar
    
    jmp can_move


    ; try to open door using matching key
try_door:
    mov al, map[bx+1]
    cmp al, 'r'
    je try_red
    cmp al, 'y'
    je try_yellow
    
    mov keypos, BlueKey
    jmp open_door

try_red:
    mov keypos, RedKey
    jmp open_door

try_yellow:
    mov keypos, YellowKey
    jmp open_door

open_door:
    push bx
    mov bx, keypos
    mov al, map[bx]
    cmp al, Key
    jne door_locked
    mov map[bx], 'X'
    mov al, 'X'
    push ax
    mov ax, 000fh
    push ax
    mov ax, bx
    push ax
    call writechar
    pop bx
    mov map[bx], 's'
    jmp can_move

door_locked:
    pop bx
    jmp cant_move


    ; teleport if destination is teleporter
teleport:
    mov al, map[bx+1]
    cmp al, 'r'
    je red_teleport
    cmp al, 'y'
    je yellow_teleport
    
    mov ax, bluetelepos[0]
    cmp ax, dx
    jne first_blue
    mov dx, bluetelepos[2]
    jmp can_move
first_blue:
    mov dx, bluetelepos[0]
    jmp can_move
    
red_teleport:
    mov ax, redtelepos[0]
    cmp ax, dx
    jne first_red
    mov dx, redtelepos[2]
    jmp can_move
first_red:
    mov dx, redtelepos[0]
    jmp can_move
    
yellow_teleport:
    mov ax, yellowtelepos[0]
    cmp ax, dx
    jne first_yellow
    mov dx, yellowtelepos[2]
    jmp can_move
first_yellow:
    mov dx, yellowtelepos[0]
    jmp can_move

    
    ; move to next level if flag is reached
finish_level:
    call nextlevelid
    jmp can_move

    
    ; try to push a rock if possible
push_rock:
    push dx
    call pushrock
    jz cant_move
    jmp can_move
    

    ; set ZF = 1 if the player cannot move to the destination
cant_move:
    xor ax, ax
    test ax, ax
    jmp return_check
    
    ; set ZF = 0 if the player can move to the destination
can_move:
    xor ax, ax
    inc ax
    
return_check:
    
    pop bx
    pop ax
    pop bp
    ret 2

endp checkmove


; check if it is possible to push the rock in the destination, and push if it is
; returns ZF = 0 if the rock is pushable and was pushed
; returns Zf = 1 if the rock is unpushable

ROCK_POS EQU bp+4
proc pushrock
    
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    
    ; get rock destination position
    mov ax, dx
    mov dx, currentpos
    sub ax, dx
    add dx, ax
    add dx, ax
    
    ; check if rock destination is an enemy (if so, it cannot be pushed)
    xor bx, bx
check_enemy:
    mov ax, enemyoffs[bx]
    cmp ax, 0
    je stop_checking_enemies
    
    mov ax, enemypos[bx]
    cmp ax, dx
    je cant_push
    
    inc bx
    jmp check_enemy

stop_checking_enemies:
    
    ; get rock destination index
    push dx
    call findmapindex
    
    ; check if rock destination is an empty cell
    mov al, map[bx]
    cmp al, ' '
    je can_push

cant_push:
    ; ZF = 1
    xor ax, ax
    test ax, ax
    jmp return_push_state

    ; mov rock to destination
can_push:
    mov map[bx], Rock
    push bx
    push dx
    call writemaptile
    mov dx, [ROCK_POS]
    push dx
    call findmapindex
    mov map[bx], ' '
    
    ; ZF = 0
    xor ax, ax
    inc ax
   
return_push_state:
    
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 2
    
endp pushrock

; get the enemy direction from character (Up/Down/Left/Right) and load it into ax
DIRECTIONCHAR EQU bp+4
proc getenemydirection
    push bp
    mov bp, sp
    
    mov al, [DIRECTIONCHAR]
    cmp al, 'u'
    je enemy_up
    cmp al, 'd'
    je enemy_down
    cmp al, 'r'
    je enemy_right
    
    mov ax, 0xFFFFh
    jmp return_direction
    
enemy_up:
    mov ax, 0xFF00h
    jmp return_direction
enemy_down:
    mov ax, 0100h
    jmp return_direction
enemy_right:
    mov ax, 0001h

return_direction:
    
    pop bp
    ret 2
    
endp getenemydirection


; check if destination FOR ENEMY is possible and change destination tile if necessary
; returns ZF = 1 if moving to destination is possible
; returns ZF = 0 if moving to destination is not possible
; returns to dx: new position
WANTED_ENEMY_POS EQU bp+4
proc checkenemymove
    
    push bp
    mov bp,sp
    push ax
    push bx
    
    ; get destination's map index
    mov dx, [WANTED_ENEMY_POS]
    push dx
    call findmapindex
    jz can_move ; move to destination if tile is not in the map (empty)
    
    mov al, map[bx] 
    
    ; change enemy's direction if reached one of these tiles:
    cmp al, Wall
    je change_direction
    cmp al, Key
    je change_direction
    cmp al, Door
    je change_direction
    cmp al, Flag
    je change_direction
    cmp al, Rock
    je change_direction
    
    cmp al, Teleporter ; teleport the enemy if destination is teleporter
    je teleport_enemy
    
    jmp keep_direction ; move to destionation if a tile doesn't require special conditions, and keep enemy's direction


    ; teleport if destination is teleporter
teleport_enemy:
    mov al, map[bx+1]
    cmp al, 'r'
    je enemy_red_teleport
    cmp al, 'y'
    je enemy_yellow_teleport
    
    mov ax, bluetelepos[0]
    cmp ax, dx
    jne enemy_first_blue
    mov dx, bluetelepos[2]
    jmp keep_direction
enemy_first_blue:
    mov dx, bluetelepos[0]
    jmp keep_direction
    
enemy_red_teleport:
    mov ax, redtelepos[0]
    cmp ax, dx
    jne first_red
    mov dx, redtelepos[2]
    jmp keep_direction
enemy_first_red:
    mov dx, redtelepos[0]
    jmp keep_direction
    
enemy_yellow_teleport:
    mov ax, yellowtelepos[0]
    cmp ax, dx
    jne first_yellow
    mov dx, yellowtelepos[2]
    jmp keep_direction
enemy_first_yellow:
    mov dx, yellowtelepos[0]
    jmp keep_direction
    

    ; set ZF = 1 if the enemy cannot move to the destination
change_direction:
    xor ax, ax
    test ax, ax
    jmp return_enemy_check
    
    ; set ZF = 0 if the enemy can move to the destination
keep_direction:
    xor ax, ax
    inc ax
    
return_enemy_check:
    
    pop bx
    pop ax
    pop bp
    ret 2

endp checkenemymove


; get the level ID of the next level
proc nextlevelid 
    
    inc LevelIDUnits
    cmp LevelIDUnits, '9'+1
    je inc_ten
    jmp updated_id

inc_ten:
    mov LevelIDUnits, '0'
    inc LevelIDTens

updated_id:
    
    ret
    
endp nextlevelid
    
 
    
end start ; set entry point and stop the assembler.