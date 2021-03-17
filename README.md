# fridge

## GET requests

    GET /situations/:name  Accept: application/json  Response: 200 Content-type: application/json
    { "situation": { "name": <String>, "temperature": <Double>, "cursor_position": <Integer> } }




## POST requests

    POST /situations/:name Response: 201 Content-type: application/json

    { "situation": { "name": <String> } }
    { "situation": { "name": <String>, "temperature": <Double>, "cursor_position": <Integer> } }


## PUT requests

    PUT  /situations/:name Response: 200 Content-type: application/json
    { "situation": { "cursor_position": <Integer> } }

## DELETE requests

    DELETE /situations/:name Response: 204 
