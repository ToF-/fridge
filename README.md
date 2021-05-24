# fridge

/              GET  display welcome message, form [name] -> /room/:name POST

/room:name     POST creates simulation for name if not existing alread, then /room:name GET
/room:name     GET  display room for name,temperature,position cursor
                    every 10 seconds : /state/:name {position} PUT with cursor position value
                                       fetch /state/:name GET and display temperature
                                       if the room.state is closed redirect to /graph/:name GET

/state/:name  GET  return room's view (temperature and position)
/state/:name  PUT  expects position   if name doesn't exist not found
                                      if name exists, change the room positionn for name

/graph/:name GET  if name doesn't exist not found
                  get the simulation for name, if room is not closed, not found
                  create the graph page for simulation render this page



