---
title: "EMA Codebook"
author: "Polina Beloborodova"
date: "3/27/2020"
output: 
  html_document: 
    fig_height: 4
    highlight: pygments
    theme: spacelab
    keep_md: true
---



## UW Codebook

### Identifiers

* `id`: ID of the experiment participant, integer
* `date`: Date when EMA survey was taken, POSIXct
* `week`: Week of EMA, integer
* `day`: Day of EMA, integer
* `time`: Time of the day, integer, labels:
    + `0`: morning
    + `1`: midday 1
    + `2`: midday 2
    + `3`: evening
* `ema_index`: Index of each EMA survey

### Measurements

__Affect__, all numeric, scale 1 to 5:

* `feel_anxious`
* `feel_depressed`
* `feel_frustrated`
* `feel_overwhelmed`
* `feel_lonely`
* `feel_happy`
* `feel_connected`

## CMU Codebook

### Identifiers

* `id` - ID of the experiment participant, integer
* `date`: Date when EMA survey was taken, POSIXct
* `week` - Week of EMA
* `day` - Day of EMA
* `time` - Time of the day, integer, labels:
    + `0`: morning
    + `1`: midday 1
    + `2`: midday 2
    + `3`: evening
* `ema_index` - Index of each EMA survey

### Measurements

__Affect__, all numeric, scale 1 to 5:

* `feel_anxious`
* `feel_depressed`
* `feel_frustrated`
* `feel_overwhelmed`
* `feel_lonely`
* `feel_happy`
* `feel_connected`

__Social interaction__, all integer:

* `int_who`: who the interaction was with, labels:
    + `1`: Family member
    + `2`: Romantic partner
    + `3`: Friend
    + `4`: Roommate
    + `5`: Classmate
    + `6`: Instructor/supervisor
    + `7`: Acquaintance
    + `8`: Other (e.g., cashier)
* `int_facetoface`: interacton was face-to-face, 0/1 (no/yes)
* `int_connected`: how connected tha participant felt during the interaction, scale 0 to 4
* `int_feel`: how the participant felt during the interaction, labels:
    + `1`: Rejected
    + `2`: Calm
    + `3`: Attentive
    + `4`: Stressed
    + `5`: Cheerful/Excited
    + `6`: Awkward
    + `7`: Valued
* `int_wouldlike`: It the participant did not interact since the last survey, how much they would like to, scale 0 to 4

__Belongingness__, integer, scale 1 to 5:

`feel_belonging`: how much the participants feels that they belong at CMY, asked in the evening only, scale 1 to 7

__Mindfulness__:

* `mindfulness`: average of three mindfulness items, numeric, scale 0 to 6
* `mindwandering`: mind wandering, integer, 0/1 (no/yes)
* `timefocus`: time focus, integer, labels:
    + `1`: Past
    + `2`: Present
    + `3`: Future

