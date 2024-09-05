function evaluate_response(data) {
    if (data.button_response == data.correct) {
        data.accuracy = "correct"
    //if (data.button_response == 0 & data.correct == 0) {
    //    data.result = "correct"
    //} else if (data.button_response == 1 & data.correct == 1) {
    //    data.result = "correct"
    //} else if (data.button_response == 2 & data.correct == 2) {
    //    data.result = "correct"
    } else  {
        data.accuracy = "incorrect"
    }
}

function data_code(data) {
    if (data.expected == "non_masc") {
        data.gender = "non_masc"
    } else if (data.expected == "masc") {
        data.gender = "masc"
    } else if (data.expected == "neutral") {
        data.gender = "neutral"
    } else {
        data.gender = "NA"
    }
}

function create_tv_array(json_object) {
    let tv_array = [];
    for (let i = 0; i < json_object.length; i++) {
        obj = {};
        obj.stimulus = json_object[i].stimulus;
        obj.data = {};
        obj.data.correct = json_object[i].correct;
        tv_array.push(obj)
    }
    return tv_array;
}

function stimObject(trial_info) { 
    obj = {};
    obj.stimulus = trial_info.stimulus;
    obj.type = jsPsychHtmlSliderResponse;
    obj.labels = ["Not at all masculine", "Very masculine"];
    console.log("Trial Type: "+obj.type)
    return obj
}

function shuffle_array(arr) {
    for (let i = arr.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        [arr[i], arr[j]] = [arr[j], arr[i]];
    }
    return arr;
}