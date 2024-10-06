const jsPsych = initJsPsych({
    show_progress_bar: true,
    override_safe_mode: true,
    auto_update_progress_bar: false,
    max_load_time: 120000, //120 seconds
    on_finish: function(data) {
        proliferate.submit({"trials": data.trials});
      }
  });

let timeline = []; //Empty timeline to which we will add trials

//PROGRESS BAR SET UP//
var count = 0; //this will increase at the end of every trial
var n_trials = 124; // 1 audio check, 108 listening trials, 13 SRQ, 1 survey q (outside survey plugin) + 1 extra for survey

//IRB//
const irb = {
    type: jsPsychHtmlButtonResponse,
    stimulus: "<p><font size='3'>DESCRIPTION: You are invited to participate in a research study. Its general purpose is to understand how people perceive speech. We are interested in how people make use of varying properties of language to infer social information about a speaker. In this study, you will hear spoken sentences, and you will be asked to make simple decisions about the sentences you hear. Following this, you will be asked to complete a short questionnaire, where you will indicate on a sliding scale how much you agree or disagree with a series of statements. You will also be asked to complete an optional demographic survey. <br><br>TIME INVOLVEMENT: Your participation will take approximately 10 to 20 minutes. <br><br>RISKS AND BENEFITS: The foreseeable risks associated with this study are minimal. This judgment is based on a large body of experience with the same or similar procedures with people of similar ages, sex, origins, etc. Study data will be stored securely, in compliance with Stanford University standards, minimizing the risk of confidentiality breach. There are no known benefits to you for participating in this study, and we cannot and do not guarantee or promise that you will receive any benefits from this study. You will help us to understand how people perceive spoken language. <br><br>PAYMENT: You will be paid at the posted rate. <br><br>PARTICIPANT RIGHTS: If you have read this form and have decided to participate in this project, please understand your participation is voluntary and you have the right to withdraw your consent or discontinue participation at any time without penalty or loss of benefits to which you are otherwise entitled. The alternative is not to participate. You have the right to refuse to answer particular questions. The results of this research study may be presented at scientific or professional meetings or published in scientific journals. Your individual privacy will be maintained in all published and written data resulting from the study. In accordance with scientific norms, the data from this study may be used or shared with other researchers for future research (after removing personally identifying information) without additional consent from you. <br><br>CONTACT INFORMATION: If you have any questions, concerns or complaints about this research study, its procedures, risks and benefits, you should contact the Protocol Director Grace Brown at (616) 498-8188. If you are not satisfied with how this study is being conducted, or if you have any concerns, complaints, or general questions about the research or your rights as a participant, please contact the Stanford Institutional Review Board (IRB) to speak to someone independent of the research team at (650) 723-2480 or toll free at 1-866-680-2906. You can also write to the Stanford IRB, Stanford University, 3000 El Camino Real, Five Palo Alto Square, 4th Floor, Palo Alto, CA 94306 USA. <br><br>WAIVER OF DOCUMENTATION: If you agree to participate in this research, please click the 'Continue' button. </font></p>",
    choices: ['Continue']
};

// push to the timeline
timeline.push(irb);

//PRELOAD AUDIO//
const preload_array = ["audio/246_SN5_high.wav","audio/246_SN20_high.wav","audio/246_SN18_high.wav","audio/246_SN19_high.wav","audio/246_SN15_high.wav","audio/246_SN9_high.wav","audio/246_SN21_mid.wav","audio/246_SN16_mid.wav","audio/246_SN4_mid.wav","audio/246_SN17_mid.wav","audio/246_SN2_mid.wav","audio/246_SN10_mid.wav","audio/246_SN1_low.wav","audio/246_SN8_low.wav","audio/246_SN3_low.wav","audio/246_SN13_low.wav","audio/246_SN7_low.wav","audio/246_SN14_low.wav","audio/340_SN5_high.wav","audio/340_SN20_high.wav","audio/340_SN18_high.wav","audio/340_SN19_high.wav","audio/340_SN15_high.wav","audio/340_SN9_high.wav","audio/340_SN21_mid.wav","audio/340_SN16_mid.wav","audio/340_SN4_mid.wav","audio/340_SN17_mid.wav","audio/340_SN2_mid.wav","audio/340_SN10_mid.wav","audio/340_SN1_low.wav","audio/340_SN8_low.wav","audio/340_SN3_low.wav","audio/340_SN13_low.wav","audio/340_SN7_low.wav","audio/340_SN14_low.wav","audio/723_SN5_high.wav","audio/723_SN20_high.wav","audio/723_SN18_high.wav","audio/723_SN19_high.wav","audio/723_SN15_high.wav","audio/723_SN9_high.wav","audio/723_SN21_mid.wav","audio/723_SN16_mid.wav","audio/723_SN4_mid.wav","audio/723_SN17_mid.wav","audio/723_SN2_mid.wav","audio/723_SN10_mid.wav","audio/723_SN1_low.wav","audio/723_SN8_low.wav","audio/723_SN3_low.wav","audio/723_SN13_low.wav","audio/723_SN7_low.wav","audio/723_SN14_low.wav","audio/246_CN1.wav","audio/246_CN2.wav","audio/246_CN3.wav","audio/246_CN4.wav","audio/246_CN5.wav","audio/246_CN7.wav","audio/246_CN8.wav","audio/246_CN9.wav","audio/246_CN10.wav","audio/246_CN13.wav","audio/246_CN14.wav","audio/246_CN15.wav","audio/246_CN16.wav","audio/246_CN17.wav","audio/246_CN18.wav","audio/246_CN19.wav","audio/246_CN20.wav","audio/246_CN21.wav","audio/340_CN1.wav","audio/340_CN2.wav","audio/340_CN3.wav","audio/340_CN4.wav","audio/340_CN5.wav","audio/340_CN7.wav","audio/340_CN8.wav","audio/340_CN9.wav","audio/340_CN10.wav","audio/340_CN13.wav","audio/340_CN14.wav","audio/340_CN15.wav","audio/340_CN16.wav","audio/340_CN17.wav","audio/340_CN18.wav","audio/340_CN19.wav","audio/340_CN20.wav","audio/340_CN21.wav","audio/723_CN1.wav","audio/723_CN2.wav","audio/723_CN3.wav","audio/723_CN4.wav","audio/723_CN5.wav","audio/723_CN7.wav","audio/723_CN8.wav","audio/723_CN9.wav","audio/723_CN10.wav","audio/723_CN13.wav","audio/723_CN14.wav","audio/723_CN15.wav","audio/723_CN16.wav","audio/723_CN17.wav","audio/723_CN18.wav","audio/723_CN19.wav","audio/723_CN20.wav","audio/723_CN21.wav","audio/gift.wav"];
const preload_trial = {
    type: jsPsychPreload,
    audio: preload_array
};

timeline.unshift(preload_trial);

//audio warning
const audio_warn = {
    type: jsPsychHtmlButtonResponse,
    choices: ['Start'],
    stimulus: "<p><font size='3'>This study requires you to listen to audio clips. To ensure you can adequately hear the audio presented in this study, the next page will have an audio attention check. Please wear headphones, and be prepared to adjust the volume on your device if necessary. <br><br> When you are ready to begin the audio attention check, click 'Start'. </font></p>",
    response_ends_trial: true,
    trial_duration: 10000
};
    
//push to the timeline
timeline.push(audio_warn);
    
//audio check
const audio_check = {
    type: jsPsychAudioButtonResponse,
    stimulus: 'audio/gift.wav',
    choices: ['dog', 'friend', 'gift', 'smile', 'blue'],
    prompt: '<p><br>This is an attention check. <br><br> Click on the word that is being repeated by the speaker.</p>',
    response_ends_trial: true,
    trial_duration: 20000,
    on_finish: function(data) {
        count++;
        var progress = count/n_trials;
        jsPsych.setProgressBar((progress));
        if (data.response == 2) {
            data.result = "correct"
        } else{
            data.result = "incorrect"
        }
    }    
};

var feedback = {
    type: jsPsychHtmlButtonResponse,
    //trial_duration: 10000,
    stimulus: function(){
      var last_trial_correct = jsPsych.data.get().last(1).values()[0].response;
      if(last_trial_correct == 2){
        return "<p>Correct! You are ready to begin the study.</p>"; // the parameter value has to be returned from the function
      } else {
        return "<p>Incorrect. Please adjust the volume of your device before beginning the study.</p>"; // the parameter value has to be returned from the function
      }
    },
    choices: ['Begin Study']
};
  
timeline.push(audio_check,feedback);

//INSTRUCTIONS//
const instructions = {
    type: jsPsychHtmlButtonResponse,
    stimulus: "<p><font size='3'>In this experiment, you will listen to a series of sentences, produced by different speakers. While listening to each sentence, you will be prompted to rate the social characteristics of its speaker. To rate the social characteristics of a speaker, click along the scale that appears on your screen. You may click along this scale after the speaker stops talking. Try to respond as quickly as you can. If you do not respond within ten seconds, the experiment will advance automatically. There will be a progress bar at the top of the screen. When you're ready to hear the first speaker, click ‘Start’. </font></p>",
    choices: ['Start']
};

//push to the timeline
timeline.push(instructions);

//define all trial lengths + arrays
let stim_array = create_tv_array(trial_obj);

const audio_trials = {
    timeline: [
        {
            type: jsPsychAudioSliderResponse,
            stimulus: jsPsych.timelineVariable('stimulus'),
            labels: ['Not at all masculine', 'Extremely masculine'],
            prompt: '<p>How masculine is the speaker of this sentence?</p>',
            response_allowed_while_playing: false,
            response_ends_trial: true,
            require_movement: true,
            slider_width: 500,
            slider_start: 5000,
            step: 1,
            min: 0,
            max: 10000,
            trial_duration: 10000,
            data: {
                spk: jsPsych.timelineVariable('speaker'),
                sib_code: jsPsych.timelineVariable('sib_code'),
                triplet_id: jsPsych.timelineVariable('triplet_id'),
                lex_code: jsPsych.timelineVariable('lex_code')
            },
            on_finish: function(data) {
                count++;
                var progress = count/n_trials;
                jsPsych.setProgressBar((progress));
            }
        },
        {
            type: jsPsychHtmlKeyboardResponse,
            choices: [""],
            stimulus: "",
            response_ends_trial: false,
            trial_duration: 500
        }
    ],
    timeline_variables: trial_obj,
    sample: {
        type: 'custom',
        fn: function(){
            //define groups
            var groups = [[0,1,2,3,4,5],[6,7,8,9,10,11],[12,13,14,15,16,17],[18,19,20,21,22,23],[24,25,26,27,28,29],[30,31,32,33,34,35],[36,37,38,39,40,41],[42,43,44,45,46,47],[48,49,50,51,52,53],[54,55,56,57,58,59],[60,61,62,63,64,65],[66,67,68,69,70,71],[72,73,74,75,76,77],[78,79,80,81,82,83],[84,85,86,87,88,89],[90,91,92,93,94,95],[96,97,98,99,100,101],[102,103,104,105,106,107]];

            //create object for each timeline variable specifying group membership and item
            var all_items = [];
            for(var i=0; i<groups.length; i++){
                for(var j=0; j<groups[i].length; j++){
                    all_items.push({group: i, trial_num:groups[i][j]});
                }
            }

            //randomize group order, with no repeat constraint
            //use custom equality function to specify what a "repeat" means
            var order = jsPsych.randomization.shuffleNoRepeats(all_items, function(a,b){
                return a.group == b.group;
            });
            
            //create the final list of trials
            return order.map(function(x){return x.trial_num});
        }
    }
};
timeline.push(audio_trials);

//INSTRUCTIONS//
const instructions_SRQ = {
    type: jsPsychHtmlButtonResponse,
    stimulus: "<p><font size='3'>You have completed the listening trials. You will now complete a short questionnaire. During the questionnaire, you will see a series of statements alongside a scale. Click along the scale to indicate how much you agree or disagree with the statement. Upon completion of the questionnaire, you will be asked to fill out an optional demographic survey. <br><br>When you're ready to begin the questionnaire, click ‘Continue’.</font></p>",
    choices: ['Continue']
};

//push to the timeline
timeline.push(instructions_SRQ);

// SRQ
let gender_array = create_tv_array(gender_objects);
const gender_ideology = {
    timeline: [
        {
            type: jsPsychHtmlSliderResponse,
            labels: ["Completely Disagree", "Completely Agree"],
            stimulus: jsPsych.timelineVariable('stimulus'),
            slider_width: 500,
            slider_start: 5000,
            min: 0,
            max: 10000,
            require_movement: true,
            response_ends_trial: true,
            trial_duration: 20000,
            data: {
                coding: jsPsych.timelineVariable('coding')
            },
            on_finish: function(data) {
                count++;
                var progress = count/n_trials;
                jsPsych.setProgressBar((progress));
            }
        }
    ],
    timeline_variables: gender_objects, //this is what is referencing the trials that were externally created
    randomize_order: true
};
timeline.push(gender_ideology);

const instructions_demo = {
    type: jsPsychHtmlButtonResponse,
    stimulus: "<p><font size='3'>You have completed the questionnaire. There will now be an optional survey. Please answer the following questions if you feel comfortable doing so. If you do not wish to answer a question, please leave it blank. <br><br>When you're ready to procede, click ‘Continue’.</font></p>",
    choices: ['Continue']
};

//push to the timeline
timeline.push(instructions_demo);

// Questionnaire pt 2
const questionnaire_2 = {
    type: jsPsychSurveyLikert,
    preamble: "Please answer the following question:",
    questions: [
      {
        prompt: "What is your political affiliation?", 
        labels: [
          "Very Progressive", 
          "Somewhat Progressive", 
          "Moderate", 
          "Somewhat Conservative", 
          "Very Conservative"
        ],
        on_finish: function(data) {
            count++;
            var progress = count/n_trials;
            jsPsych.setProgressBar((progress));
        }
      }
    ]
};
timeline.push(questionnaire_2);

//Survey
const questionnaire = {
    type: jsPsychSurvey,
    pages: [
        [
            {
                type: 'html',
                prompt: "Please answer the following questions:"
            },
            {
                type: 'multi-choice',
                prompt: 'Did you read the instructions and do you think you did the task correctly?', 
                name: 'correct', 
                options: ['Yes', 'No', 'I was confused']
            },
            {
                type: 'drop-down',
                prompt: 'Gender:',
                name: 'gender',
                options: ['Female', 'Male', 'Non-binary/Non-conforming', 'Other']
            },
            {
                type: 'text',
                prompt: 'Age:',
                name: 'age',
                textbox_columns: 10
            },
            {
                type: 'drop-down',
                prompt: 'Level of education:',
                name: 'education',
                options: ['Some high school', 'Graduated high school', 'Some college', 'Graduated college', 'Hold a higher degree']
            },
            {
                type: 'text',
                prompt: "Native language? (What was the language spoken at home when you were growing up?)",
                name: 'language',
                textbox_columns: 20
            },
            {
                type: 'drop-down',
                prompt: 'Where in the U.S. do you live?',
                name: 'region',
                options: ['Midwest - IA, IL, IN, KS, MI, MN, MO, ND, NE, OH, SD, WI', 'Northeast - CT, DC, DE, MA, MD, ME, NH, NJ, NY, PA, RI, VT', 'Southeast - AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV', 'Southwest - AZ, NM, OK, TX', 'West - AK, CA, CO, HI, ID, MT, NV, OR, UT, WA, WY']
            },
            {
                type: 'drop-down',
                prompt: 'Do you think the payment was fair?',
                name: 'payment',
                options: ['The payment was too low', 'The payment was fair']
            },
            {
                type: 'drop-down',
                prompt: 'Did you enjoy the experiment?',
                name: 'enjoy',
                options: ['Worse than the average experiment', 'An average experiment', 'Better than the average experiment']
            },
            {
                type: 'text',
                prompt: "Do you have any other comments about this experiment?",
                name: 'comments',
                textbox_columns: 30,
                textbox_rows: 4
            }
        ]
    ],
    on_finish: function(){
        jsPsych.setProgressBar(1); // set progress bar to full.
    }
};
timeline.push(questionnaire);

// THANKS //
const thanks = {
    type: jsPsychHtmlButtonResponse,
    choices: ['Finish'],
    stimulus: "Thank you for your time! Please click 'Finish' and then wait a moment until you're directed back to Prolific.<br><br>"
};
timeline.push(thanks);

//RUN//
jsPsych.run(timeline);