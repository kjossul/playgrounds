function stampaData() {
    data = new Date();  // nuovo oggetto "data"
    giorno = data.getDate();
    mese = data.getMonth() + 1;  // gennaio == 0
    anno = data.getFullYear();
    stringa = `${giorno}/${mese}/${anno}`;  // template string
    alert(stringa);
}

function chiediConferma() {
    var risposta = confirm("Acconsenti all'utilizzo dei cookies?");
    if (!risposta) {
        // l'utente ha premuto su "annulla"
        location.assign("http://www.unito.it");
    }
}

function manipolaStringa(stringa) {
    alert(stringa.toUpperCase());
}

var vittorie = 0;
var sconfitte = 0;

function gioco() {
    var numero = prompt("Scegli un numero da 1 a 10");
    // Random torna un numero in [0, 1), floor() arrotonda il numero all'intero più piccolo. Quindi dopo la floor()
    // avrò un numero da 0 a 9 (10 non può essere perché la random() non include 1), quindi mi basta aggiungere 1.
    var rand = Math.floor(Math.random() * 10) + 1;
    if (numero === rand.toString()) {
        vittorie += 1;
    } else {
        sconfitte += 1;
    }
    alert(`Vittorie: ${vittorie}\nSconfitte: ${sconfitte}`);
}