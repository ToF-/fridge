# fridge

## GET requests

    /      ---> display: the fridge game, message, _name_, [start]
    start ---> POST room/ { name }

    /room/:name  ---> if room state is STARTED: display: name, temperature, _command_
                                                every 60s ---> PUT room/:name { command: integer } then GET room/:name
                      if room state is FINAL: display: name, table { time, temp, command } [CSV]
                                                CSV ---> /room/csv/:name

    /room/csv/:name ---> raw csv text of history

## POST requests

    room/ { name } ---> if not existing, not FINAL ---> room creation, then GET room/:name
                        if existing ---> GET room/:name
                        if FINAL ---> GET room/:name

## PUT requests

    room/:name { command:integer } ---> if existing, not FINAL ---> update room, then GET room/:name
