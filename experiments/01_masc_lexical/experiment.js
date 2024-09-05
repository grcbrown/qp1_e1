const jsPsych = initJsPsych({
    show_progress_bar: true,
    auto_update_progress_bar: false,
    on_finish: function(data) {
        proliferate.submit({"trials": data.trials});
      }
  });

let timeline = []; //Empty timeline to which we will add trials

//IRB//
const irb = {
    type: jsPsychHtmlButtonResponse,
    stimulus: '<p><font size="3">DESCRIPTION: You are invited to participate in a research study. Its general purpose is to understand how people perceive written stimuli. You will be asked to read a series of sentences and provide judgments about the social characteristics of the speakers of these sentences. <br><br>RISKS AND BENEFITS: There are no known risks, costs, or discomforts in this study and this judgment is based on a large body of experiments with the same or similar procedures with people of similar ages, sex, origins, etc.  We cannot and do not guarantee or promise that you will receive any benefits from this study. You will help us to understand how people perceive written stimuli.<br><br>PAYMENTS: You will be paid at the posted rate. <br><br>SUBJECT RIGHTS: If you have read this form and have decided to participate in this project, please understand your participation is voluntary and you have the right to withdraw your consent or discontinue participation at any time without penalty or loss of benefits to which you are otherwise entitled. You have the right to refuse to answer particular questions. Your individual privacy will be maintained in all published and written data resulting from the study. <br><br> CONTACT INFORMATION: If you have any questions, concerns or complaints about this research study, its procedures, risks and benefits, you should contact the Protocol Director Meghan Sumner at (650)-725-9336. If you are not satisfied with how this study is being conducted, or if you have any concerns, complaints, or general questions about the research or your rights as a participant, please contact the Stanford Institutional Review Board (IRB) to speak to someone independent of the research team at (650)-723-2480 or toll free at 1-866-680-2906. You can also write to the Stanford IRB, Stanford University, 3000 El Camino Real, Five Palo Alto Square, 4th Floor, Palo Alto, CA 94306 USA.<br><br>WAIVER OF DOCUMENTATION: If you agree to participate in this research, please continue on to the next page.</font></p>',
    choices: ['Continue']
};

// push to the timeline
timeline.push(irb);

//INSTRUCTIONS//
const instructions = {
    type: jsPsychHtmlButtonResponse,
    stimulus: "<p><font size='3'>In this experiment, you will read a series of sentences. Each sentence has been produced by a different speaker. After reading each sentence, you will be asked to rate the social characteristics of its speaker. To rate the social characteristics of a speaker, click along the scale. After providing your rating, you can proceed to the next trial by clicking 'Continue'. Some trials will have comprehension questions. To complete these, click on the button with the correct answer. <br><br>When you're ready to begin, click ‘Start’.</font></p>",
    choices: ['Start']
};

//push to the timeline
timeline.push(instructions);

let trial_objects_shuffle = jsPsych.randomization.shuffle(trial_objects);
for (let i = 0; i < trial_objects_shuffle.length; i++) {
    const trial = {
        type: jsPsychHtmlVasResponse,
        labels: ["Not at all masculine", "Extremely masculine"],
        stimulus: trial_objects_shuffle[i].stimulus,
        scale_width: 500,
        ticks: false,
        required: true,
        //response_ends_trial: true,
        trial_duration: 10000,
        data: {
            expected: trial_objects_shuffle[i].expected,
            triplet_id: trial_objects_shuffle[i].triplet_id
        },
        on_finish: function(data) {
            jsPsych.setProgressBar((data.trial_index - 1) / (timeline.length + trial_objects_shuffle.length));
        }
    }
    timeline.push(trial)
    if (trial_objects_shuffle[i].triplet_id < 9) {
         let attn_info = {
             type: jsPsychHtmlMultiResponse,
             stimulus: trial_objects_shuffle[i].comp,
             prompt: "<br><br>Click on the correct answer.",
             button_choices: trial_objects_shuffle[i].comp_button,
             data: {
                correct: trial_objects_shuffle[i].correct
            },
             trial_duration: 10000,
             //response_ends_trial: true,
             on_finish: function(data) {
                jsPsych.setProgressBar((data.trial_index - 1) / (timeline.length + trial_objects_shuffle.length));
                evaluate_response(data);
            }
         }
         timeline.push(attn_info)
    }
};

//INSTRUCTIONS//
const instructions_SRQ = {
    type: jsPsychHtmlButtonResponse,
    stimulus: "<p><font size='3'>You have completed the experimental trials. You will now complete a short questionnaire. During the questionnaire, you will see a series of statements alongside a scale. Click along the scale to indicate how much you agree or disagree with the statement. Upon completion of the questionnaire, you will be asked to fill out an optional demographic survey. <br><br>When you're ready to begin the questionnaire, click ‘Continue’.</font></p>",
    choices: ['Continue']
};

//push to the timeline
timeline.push(instructions_SRQ);

// SRQ
let gender_array = create_tv_array(gender_objects);
const gender_ideology = {
    timeline: [
        {
            type: jsPsychHtmlVasResponse,
            labels: ["Completely Disagree", "Completely Agree"],
            stimulus: jsPsych.timelineVariable('stimulus'),
            scale_width: 500,
            ticks: false,
            required: true,
            //response_ends_trial: true,
            trial_duration: 10000,
            data: {
                coding: jsPsych.timelineVariable('coding')
            },
            on_finish: function(data) {
                jsPsych.setProgressBar((data.trial_index - 1) / (timeline.length + gender_array.length));
            }
        }
    ],
    timeline_variables: gender_objects, //this is what is referencing the trials that were externally created
    randomize_order: true
};
timeline.push(gender_ideology);

const instructions_demo = {
    type: jsPsychHtmlButtonResponse,
    stimulus: "<p><font size='3'>You have completed the questionnaire. There will now be an optional survey. Please answer the following questions if you feel comfortable doing so. <br><br>When you're ready to procede, click ‘Continue’.</font></p>",
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
        ]
      }
    ]
};
timeline.push(questionnaire_2);

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
        jsPsych.setProgressBar(1); // set progress bar to 85% full.
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