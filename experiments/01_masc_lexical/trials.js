let trial_objects = [
    {
        "stimulus": ['<p style="font-weight:bold;">'+"Can I take a look at your answer? "+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "masc",
        "comp": "What did the speaker ask to see?<br><br>", 
        "comp_button": ['The test','The answer','The letter'],
        "correct": 1,
        "triplet_id": 1
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"Dude, you won't believe who I bumped into this week."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "masc", 
        "comp": "True or False: The speaker expressed that they bumped into someone two weeks ago.<br><br>",
        "comp_button": ['True', 'False'],
        "correct": 1,
        "triplet_id": 2
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"When is lunch? I'm seriously starving."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "masc", 
        "comp": "What meal is the speaker asking about?<br><br>",
        "comp_button": ['Breakfast', 'Lunch', 'Dinner'],
        "correct": 1,
        "triplet_id": 3
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"I see you got new purple mugs."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "masc", 
        "comp": "What type of dish is the speaker commenting on?<br><br>",
        "comp_button": ['Mug', 'Plate', 'Bowl'],
        "correct": 0,
        "triplet_id": 4
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"Without a map, we are fully lost."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "masc", 
        "comp": "True or False: The speaker believes they will be lost without a GPS.<br><br>",
        "comp_button": ['True','False'],
        "correct": 1,
        "triplet_id": 5
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"Chase was pretty tired on Monday."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "masc", 
        "comp": "Who was tired?<br><br>",
        "comp_button": ['Sara','Tommy','Chase'],
        "correct": 2,
        "triplet_id": 6
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"At the beach, I took the coolest picture of the sunset."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "masc", 
        "comp": "What did the speaker photograph?<br><br>",
        "comp_button": ['The sunset','The beach','The moon'],
        "correct": 0,
        "triplet_id": 7
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"I bought a blue shirt last Friday."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "masc", 
        "comp": "What color shirt did the speaker purchase?<br><br>",
        "comp_button": ['Red','Blue','Yellow'],
        "correct": 1,
        "triplet_id": 8
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"I hear what you're saying, darling."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "non_masc", 
        "comp": "True or false: The speaker referred to the listener as 'dude'.<br><br>",
        "comp_button": ['True','False'],
        "correct": 1,
        "triplet_id": 9
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"I can't stop giggling -- that was really funny."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "non_masc", 
        "comp": "True or false: The speaker was laughing.<br><br>",
        "comp_button": ['True','False'],
        "correct": 0,
        "triplet_id": 10
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"My workout yesterday had me feeling cute."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "non_masc", 
        "comp": "What did the speaker do?<br><br>",
        "comp_button": ['Work out','Go dancing','Watch a movie'],
        "correct": 0,
        "triplet_id": 11
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"My sister just wanted to let you know that she will be late. "+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "non_masc", 
        "comp": "Who will be late?<br><br>",
        "comp_button": ["The speaker","The speaker's brother","The speaker's sister"],
        "correct": 2, 
        "triplet_id": 12
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"I bought an amber shirt last Friday."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "non_masc", 
        "comp": "What color shirt did the speaker purchase?<br><br>",
        "comp_button": ['Maroon','Periwinkle','Amber'],
        "correct": 2,
        "triplet_id": 13
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"I'm surprised how yummy that burger was."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "non_masc", 
        "comp": "What did the speaker eat?<br><br>",
        "comp_button": ['A burger','A steak','A sandwich'],
        "correct": 0,
        "triplet_id": 14
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"Some people think that show was super entertaining."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "non_masc", 
        "comp": "True or False: Everyone thought the show was boring.<br><br>",
        "comp_button": ['True','False'],
        "correct": 1,
        "triplet_id": 15
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"My office is terribly boring."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "non_masc", 
        "comp": "What did the speaker find boring?<br><br>",
        "comp_button": ['The bedroom','The office','The party'],
        "correct": 1,
        "triplet_id": 16
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"I'm surprised how tasty that taco was."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "neutral", 
        "comp": "What did the speaker eat?<br><br>",
        "comp_button": ['A taco','A waffle','An apple'],
        "correct": 0,
        "triplet_id": 17
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"Your presentation sounded great."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "neutral", 
        "comp": "Did the speaker enjoy the presentation?<br><br>",
        "comp_button": ['Yes','No'],
        "correct": 0,
        "triplet_id": 18
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"The party last night was really cool."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "neutral", 
        "comp": "When did the speaker attend a party?<br><br>",
        "comp_button": ['Last month','Last week','Last night'],
        "correct": 2,
        "triplet_id": 19
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"That's a funny thing to say."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "neutral", 
        "comp": "True or False: The speaker was angry.<br><br>",
        "comp_button": ['True','False'],
        "correct": 1,
        "triplet_id": 20
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"My bad, I didn't see you there."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "neutral", 
        "comp": "True or False: The speaker apologized.<br><br>",
        "comp_button": ['True','False'],
        "correct": 0,
        "triplet_id": 21
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"I feel sorry for the old thing."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "neutral", 
        "comp": "True or False: The speaker discussed what they ate for dinner.<br><br>",
        "comp_button": ['True','False'],
        "correct": 1,
        "triplet_id": 22
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"I hear what you're saying, Sam."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "neutral", 
        "comp": "True or False: The speaker talked about their dog.<br><br>",
        "comp_button": ['True','False'],
        "correct": 1,
        "triplet_id": 23
    },
    {
        "stimulus": ['<p style="font-weight:bold;">'+"I think that article was well-researched."+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"],
        "expected": "neutral", 
        "comp": "What did the speaker think was well-researched?<br><br>",
        "comp_button": ['The book','The show','The article'],
        "correct": 2,
        "triplet_id": 24
    },
]

//template: ['<p style="font-weight:bold;">'+'STIMULUS'+'</p>'+"<br><br>How masculine is the speaker of this sentence?<br><br>"]

let gender_objects = [
    {
        "stimulus": "People can be both aggressive and nurturing regardless of sex.<br><br>",
        "coding": "NEGATIVE"
    },
    {
        "stimulus": "People should be treated the same regardless of their sex.<br><br>",
        "coding": "NEGATIVE"
    },
    {
        "stimulus": "The freedom that children are given should be determined by their age and maturity level and not by their sex.<br><br>",
        "coding": "NEGATIVE"
    },
    {
        "stimulus": "Tasks around the house should not be assigned by sex.<br><br>",
        "coding": "NEGATIVE"
    },
    {
        "stimulus": "We should stop thinking about whether people are male or female and focus on other characteristics.<br><br>",
        "coding": "NEGATIVE"
    },
    {
        "stimulus": "A father’s major responsibility is to provide financially for his children.<br><br>",
        "coding": "POSITIVE"
    },
    {
        "stimulus": "Men are more sexual than women.<br><br>",
        "coding": "POSITIVE"
    },
    {
        "stimulus": "Some types of work are just not appropriate for women.<br><br>",
        "coding": "POSITIVE"
    },
    {
        "stimulus": "Mothers should make most decisions about how children are brought up.<br><br>",
        "coding": "POSITIVE"
    },
    {
        "stimulus": "Mothers should work only if necessary.<br><br>",
        "coding": "POSITIVE"
    },
    {
        "stimulus": "Girls should be protected and watched over more than boys.<br><br>",
        "coding": "POSITIVE"
    },
    {
        "stimulus": "Only some types of work are appropriate for both men and women.<br><br>",
        "coding": "POSITIVE"
    },
    {
        "stimulus": "For many important jobs, it is better to choose men instead of women.<br><br>",
        "coding": "POSITIVE"
    }
]