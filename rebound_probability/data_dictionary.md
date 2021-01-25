Data Dictionary
================

# Problem

In basketball and many other sports, the goal of the game is to score
more points than the other team. One way to do this is to limit the
other team’s possessions. This could potentially limit your team’s
possessions. Thus your possessions to the other team’s possessions will
be 1:1. Offensive rebounds are a way to increase this ratio in your
favor, 2:1,3:1,…,N:1. What if we were able to determine the chance of us
getting an offensive rebound? Then we could use the necessary tactics to
increase our chances of getting an offensive rebound if they were low.

# Play-by-Play

name - description game\_id - ID for the game playbyplayorder\_id - ID
for the play by play event in this row row\_type - Type of play by play
event, shot or final free throw (e.g. the last free throw taken during a
trip to the line that could be rebounded if missed) F.OREB - Whether a
shot was offensive rebounded (Yes or No) REBOFFENSIVE - Whether a shot
was offensive rebounded (0 = no, 1 = yes) REBDEFFENSIVE - Whether a shot
was defensive rebounded (0 = no, 1 = yes) OFF\_TEAM\_ID - Team ID of the
team on offense DEF\_TEAM\_ID - Team ID of the team on defense
HOME\_TEAM\_ID - Team ID of the home team AWAY\_TEAM\_ID - Team ID of
the away team FTMADE - Did this play result in a made free throw? (0 =
no, 1 = yes) FTMISSED - Did this play result in a missed free throw? (0
= no, 1 = yes) FG2MADE - Did this play result in a made 2 point attempt?
(0 = no, 1 = yes) FG2MISSED - Did this play result in a missed 2 point
attempt? (0 = no, 1 = yes) FG2ATTEMPTED - Was a two point shot attempted
on this play? (0 = no, 1 = yes) FG3MADE - Did this play result in a made
3 point attempt? (0 = no, 1 = yes) FG3MISSED - Did this play result in a
missed 3 point attempt? (0 = no, 1 = yes) FG3ATTEMPTED - Was a three
point shot attempted on this play? (0 = no, 1 = yes) SECONDCHANCE -
Indicator for a second chance opportunity. Increments for each
subsequent chance following another offensive rebound. (e.g. if there
are 3 shots on a possession, the first will have 0, the second will have
1,the third will have 2) ANDONE - Was this play a shooting foul on made
shot? (0 = no, 1 = yes) INBONUS - Is the offensive team in the bonus? (0
= no, 1 = yes) EVENTDESCRIPTION - A description of the play by play
event ACTIONDESCRIPTION - A more specific description of the play by
play event SHOOTER\_PLAYER\_ID - The player\_id of the shooter
REB\_PLAYER\_ID - The player\_id of the rebounder
PLAYER\_ID\_OFF\_PLAYER\_1 - The ID of one of the 5 offensive players on
the court (no particular order) PLAYER\_ID\_OFF\_PLAYER\_2 - The ID of
one of the 5 offensive players on the court (no particular order)
PLAYER\_ID\_OFF\_PLAYER\_3 - The ID of one of the 5 offensive players on
the court (no particular order) PLAYER\_ID\_OFF\_PLAYER\_4 - The ID of
one of the 5 offensive players on the court (no particular order)
PLAYER\_ID\_OFF\_PLAYER\_5 - The ID of one of the 5 offensive players on
the court (no particular order) PLAYER\_ID\_DEF\_PLAYER\_1 - The ID of
one of the 5 defensive players on the court (no particular order)
PLAYER\_ID\_DEF\_PLAYER\_2 - The ID of one of the 5 defensive players on
the court (no particular order) PLAYER\_ID\_DEF\_PLAYER\_3 - The ID of
one of the 5 defensive players on the court (no particular order)
PLAYER\_ID\_DEF\_PLAYER\_4 - The ID of one of the 5 defensive players on
the court (no particular order) PLAYER\_ID\_DEF\_PLAYER\_5 - The ID of
one of the 5 defensive players on the court (no particular order)

# Data Location

name - description GAME\_ID - ID for the game PLAYBYPLAYORDER\_ID - ID
for the play by play event in this row. ROW\_TYPE - Type of play by play
event, shot or final free throw (e.g. the last free throw taken during a
trip to the line that could be rebounded if missed)
ATSHOT\_LOC\_X\_OFF\_PLAYER\_1 - The x coordinate of the location of
Offensive Player 1 at the time of the shot
ATSHOT\_LOC\_Y\_OFF\_PLAYER\_1 - The y coordinate of the location of
Offensive Player 1 at the time of the shot ATRIM\_LOC\_X\_OFF\_PLAYER\_1
- The x coordinate of the location of Offensive Player 1 at the time the
ball hit the rim ATRIM\_LOC\_Y\_OFF\_PLAYER\_1 - The y coordinate of the
location of Offensive Player 1 at the time the ball hit the rim
ATSHOT\_LOC\_X\_OFF\_PLAYER\_2 - The x coordinate of the location of
Offensive Player 1 at the time of the shot
ATSHOT\_LOC\_Y\_OFF\_PLAYER\_2 - The y coordinate of the location of
Offensive Player 1 at the time of the shot ATRIM\_LOC\_X\_OFF\_PLAYER\_2
- The x coordinate of the location of Offensive Player 1 at the time the
ball hit the rim ATRIM\_LOC\_Y\_OFF\_PLAYER\_2 - The y coordinate of the
location of Offensive Player 1 at the time the ball hit the rim
ATSHOT\_LOC\_X\_OFF\_PLAYER\_3 - The x coordinate of the location of
Offensive Player 1 at the time of the shot
ATSHOT\_LOC\_Y\_OFF\_PLAYER\_3 - The y coordinate of the location of
Offensive Player 1 at the time of the shot ATRIM\_LOC\_X\_OFF\_PLAYER\_3
- The x coordinate of the location of Offensive Player 1 at the time the
ball hit the rim ATRIM\_LOC\_Y\_OFF\_PLAYER\_3 - The y coordinate of the
location of Offensive Player 1 at the time the ball hit the rim
ATSHOT\_LOC\_X\_OFF\_PLAYER\_4 - The x coordinate of the location of
Offensive Player 1 at the time of the shot
ATSHOT\_LOC\_Y\_OFF\_PLAYER\_4 - The y coordinate of the location of
Offensive Player 1 at the time of the shot ATRIM\_LOC\_X\_OFF\_PLAYER\_4
- The x coordinate of the location of Offensive Player 1 at the time the
ball hit the rim ATRIM\_LOC\_Y\_OFF\_PLAYER\_4 - The y coordinate of the
location of Offensive Player 1 at the time the ball hit the rim
ATSHOT\_LOC\_X\_OFF\_PLAYER\_5 - The x coordinate of the location of
Offensive Player 1 at the time of the shot
ATSHOT\_LOC\_Y\_OFF\_PLAYER\_5 - The y coordinate of the location of
Offensive Player 1 at the time of the shot ATRIM\_LOC\_X\_OFF\_PLAYER\_5
- The x coordinate of the location of Offensive Player 1 at the time the
ball hit the rim ATRIM\_LOC\_Y\_OFF\_PLAYER\_5 - The y coordinate of the
location of Offensive Player 1 at the time the ball hit the rim
ATSHOT\_LOC\_X\_DEF\_PLAYER\_1 - The x coordinate of the location of
defensive Player 1 at the time of the shot
ATSHOT\_LOC\_Y\_DEF\_PLAYER\_1 - The y coordinate of the location of
defensive Player 1 at the time of the shot ATRIM\_LOC\_X\_DEF\_PLAYER\_1
- The x coordinate of the location of defensive Player 1 at the time the
ball hit the rim ATRIM\_LOC\_Y\_DEF\_PLAYER\_1 - The y coordinate of the
location of defensive Player 1 at the time the ball hit the rim
ATSHOT\_LOC\_X\_DEF\_PLAYER\_2 - The x coordinate of the location of
defensive Player 1 at the time of the shot
ATSHOT\_LOC\_Y\_DEF\_PLAYER\_2 - The y coordinate of the location of
defensive Player 1 at the time of the shot ATRIM\_LOC\_X\_DEF\_PLAYER\_2
- The x coordinate of the location of defensive Player 1 at the time the
ball hit the rim ATRIM\_LOC\_Y\_DEF\_PLAYER\_2 - The y coordinate of the
location of defensive Player 1 at the time the ball hit the rim
ATSHOT\_LOC\_X\_DEF\_PLAYER\_3 - The x coordinate of the location of
defensive Player 1 at the time of the shot
ATSHOT\_LOC\_Y\_DEF\_PLAYER\_3 - The y coordinate of the location of
defensive Player 1 at the time of the shot ATRIM\_LOC\_X\_DEF\_PLAYER\_3
- The x coordinate of the location of defensive Player 1 at the time the
ball hit the rim ATRIM\_LOC\_Y\_DEF\_PLAYER\_3 - The y coordinate of the
location of defensive Player 1 at the time the ball hit the rim
ATSHOT\_LOC\_X\_DEF\_PLAYER\_4 - The x coordinate of the location of
defensive Player 1 at the time of the shot
ATSHOT\_LOC\_Y\_DEF\_PLAYER\_4 - The y coordinate of the location of
defensive Player 1 at the time of the shot ATRIM\_LOC\_X\_DEF\_PLAYER\_4
- The x coordinate of the location of defensive Player 1 at the time the
ball hit the rim ATRIM\_LOC\_Y\_DEF\_PLAYER\_4 - The y coordinate of the
location of defensive Player 1 at the time the ball hit the rim
ATSHOT\_LOC\_X\_DEF\_PLAYER\_5 - The x coordinate of the location of
defensive Player 1 at the time of the shot
ATSHOT\_LOC\_Y\_DEF\_PLAYER\_5 - The y coordinate of the location of
defensive Player 1 at the time of the shot ATRIM\_LOC\_X\_DEF\_PLAYER\_5
- The x coordinate of the location of defensive Player 1 at the time the
ball hit the rim ATRIM\_LOC\_Y\_DEF\_PLAYER\_5 - The y coordinate of the
location of defensive Player 1 at the time the ball hit the rim

# Player Position

name - description PLAYER\_ID - An ID identifying the player POSITION -
The player’s primary position POSITION2 - The player’s secondary
position (if applicable)

# Player Rebounding

name - description PLAYER\_ID - An ID identifying the player GAMES - The
number of games this player played in the games that are included in
both the training and testing data. MINUTES -The number of minutes this
player played in the games that are included in both the training and
testing data. OFF\_REBOUNDS - The number of offensive rebounds this
player grabbed in the games that are included in both the training and
testing data. DEF\_REBOUNDS - The number of defensive rebounds this
player grabbed in the games that are included in both the training and
testing data. OFF\_REBCHANCES - The number of chances this player had to
secure an offensive rebound during the games that are included in both
the training and testing data. Note that a player simply needs to be on
offense for a missed shot to record an “off\_rebchance”. This variable
does not indicate, e.g., whether the player was in position to
potentially secure a rebound. DEF\_REBCHANCES - The number of chances
this player had to secure a defensive rebound during the games that are
included in both the training and testing data. Note that a player
simply needs to be on defense for a missed shot to record a
“def\_rebchance”. This variable does not indicate, e.g., whether the
player was in position to potentially secure a rebound.
