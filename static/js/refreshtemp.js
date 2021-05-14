async function getTemperature() {
    let nameElement = document.getElementById("name");
    let name = nameElement.innerHTML;
    let tempElement = document.getElementById("temperature");
    fetch('rooms/'+name)
    .then(function(response) {
        if (response.status !== 200) {
            console.log('error fetching ' + 'rooms/' + name + ' status: ' + response.status);
            return;
        }
        response.json().then(function(room) {
            tempElement.innerHTML = room.temperature;
        });
    }
    )
    .catch(function(err) {
        console.log('Fetch Error :-S', err);
    });

}

function setRepeatedRefresh() {
    window.setInterval(getTemperature, 3000);
}
