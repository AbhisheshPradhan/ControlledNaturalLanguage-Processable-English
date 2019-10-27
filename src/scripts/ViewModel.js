/**
 *   Author: Rudz Evan Bernardez, Macquarie University
 *   File Name: ViewModel.js
 *
 */

var viewModel = {
    $text_field: $("#text_field"),
    $saveFileName: $("#saveFileName"),
    textAreaStr: ko.observable(""), 
    processedInput: ko.observable(""),// For display 
    result: ko.observable(""),
    // firstIndexOfCurrentWord: 0, //Need to use this for backspace 
    token: ko.observable(""),
    textList: ko.observableArray([]),
    smallAsp: "",
    bigAsp: "",
    aspState: false,
    allowInput: true,
    catTable: ko.observableArray([]),
    anaExp: ko.observableArray([]),
    reasonerMode: "normal", // default settings
    inputMode: ko.observable("Text Mode"), //default settings
    asp: ko.observable(''),
    answer: ko.observable(''),

    fileNames: ko.observableArray([]),
    initialLoad: true,
    isEndOfSentence: false,

    $loading: $("#loader-overlay"),

    lookaheadObject: ko.observableArray([]),
    initLookUpObj: [], //initial lookup obj
    lookUpTable: ko.observableArray([]),
    initLookUpTable: [], //initial lookuptable

    currentInitialLookUpTable: [], //used in backspace handler

    isDropdownInput: false,
    isLoadFileInput: false,

    init: function () {
        console.log("init should be only called once or after sentence completion")
        var lastNodePostedWasBlank = textLineData.nodes[textLineData.nodes.length - 1] != " ";
        var textAreaEmpty = this.textAreaStr().length == 0;
        if (textAreaEmpty && lastNodePostedWasBlank) {
            this.updateViewForWord(" ");
            this.initLookUpTable = this.lookUpTable();
            this.initLookUpObj = this.lookaheadObject();
        }
    },

    loadLookahead: function () {
        // console.log("loadLookahead clicked")
        if (this.initialLoad) {
            expressionLoader.loadLookahead();
            this.init();
            this.initialLoad = false;
        }
    },

    postLookaheadWord: function (data, event) {
        expressionLoader.postLookaheadWord(data, event);
    },

    postAnaExpClicked: function (word) {
        expressionLoader.postAnaExpClicked(word);
    },

    updateLookUpTable: function () {
        expressionLoader.updateLookUpTable();
    },

    populateLookUpTable: function (data) {
        expressionLoader.populateLookUpTable(data);
    },

    updateViewForWord(word) {
        var request = $.when(token.postToken(word));
        request.done(function (data) {
            var json = JSON.parse(data);
            if (json.hasOwnProperty('spelling suggestions') || (json.lookahead.length == 0 && !json.hasOwnProperty('asp'))) {
                viewModel.allowInput = false;
                var lAhead = lookaheadObj.createLookaheadTable(lookaheadObj);
                lAhead = lookaheadObj.addStrInHeadForEachCatInLookahead(word, lAhead);
                viewModel.lookaheadObject(lAhead);
                viewModel.anaExp(json.ana);
            } else {
                viewModel.populateLookUpTable(json);
                expressionLoader.loadLookahead();
                viewModel.anaExp(json.ana);
                viewModel.allowInput = true;
            }
            
            if (word == "." || word == "?") {
                var sentences = (viewModel.textAreaStr().trim().slice(0, viewModel.textAreaStr().length) + word)
                // remove extra '.'s when there are consecutive .'s e.g .. = .
                var regexReplaceFullStop = /(^[\.\s]*)|([\s\.]*(?=(\.|\))))|(\s*\([\.\s]*\)\s*\.)|(\s*(?=\())/g;
                sentences = sentences.replace(regexReplaceFullStop, "").trim();

                var regexReplaceQuestionMark = /(^[\?\s]*)|([\s\?]*(?=(\?|\))))|(\s*\([\?\s]*\)\s*\?)|(\s*(?=\())/g;
                sentences = sentences.replace(regexReplaceQuestionMark, "").trim();

                sentences = sentences.replace(/\.(?!\d)|([^\d])\.(?=\d)/g, '$1.|');
                sentences = sentences.replace(/\?(?!\d)|([^\d])\?(?=\d)/g, '$1?|');

                var sentencesArray = sentences.split("|");
                sentencesArray.pop(); //remove "" at the end of the array

                textLineData.addSentence((sentencesArray.pop()).trim());

                viewModel.setAsp(json);
                viewModel.setAnswer(json.answer);

                if(viewModel.isDropdownInput || viewModel.isLoadFileInput) {
                    viewModel.updateViewForWord(" ");
                }
            }

            if(textLineData.lastSentenceNodes().length > 1) {
                viewModel.processedInput(textLineData.lastSentenceNodes().join(" "));
            } else if(textLineData.sentences.length > 0) {
                viewModel.processedInput(textLineData.sentences[textLineData.sentences.length - 1]);
            }

            if (json.hasOwnProperty('ana') && word != "." && json.ana.length != 0) {
                var temp = json.ana;
                for (i = 0; i < json.ana.length; i++) {
                    if (json.ana[i].length > 1) {
                        temp[i] = [temp[i].join(" ")];

                    } else {
                        temp[i] = json.ana[i];
                    }
                }
                viewModel.anaExp(temp);
            }
        });
        console.log("nodes", textLineData.nodes);
    },

    // NAVBAR functions

    // Loads the list of file names in texts folder
    loadFileNames: function () {
        if (this.initialLoad) {
            expressionLoader.loadLookahead();
            this.init();
            this.initialLoad = false;
        }
        navBar.loadFileNames();
    },

    // Load a single file name
    loadFile: function () {
        var loadedFileName = this;
        navBar.loadFile(loadedFileName);
    },

    generateText: function () {
        navBar.generateText();
    },

    saveFile() {
        navBar.saveButton();
    },


    // EVENT HANDLER functions

    // Text submitted
    onSubmit: function () {
        console.log("onSubmit")
        eventHandler.enterKey();
    },

    // Char entered
    onKeyPress: function (d, e) {
        return eventHandler.keyUpdate(d, e);
    },

    // Backspace detected
    onKeyUp: function (d, e) {
        var keyVal = e.keyCode;
        if (keyVal == 8) {
            eventHandler.backspace();
        }
    },

    // RESULTS section functions

    setAnswer: function (ansData) {
        results.setAnswer(ansData);
    },

    setAsp: function (aspData) {
        results.setAsp(aspData);
    }
};

ko.applyBindings(viewModel);


// Prevent default behavior of the text area html element
(function ($) {
    $(document).ready(function () {
        $("#text_field").keypress(function (e) {
            if (e.keyCode == 13 && !e.shiftKey) {
                e.preventDefault();
                return false;
            }
        });
        $("#text_field").click(function (e) {
            console.log("text field clicked");
            e.preventDefault();
        });
        $("#text_field").keyup(function (e) {
            if (e.keyCode == 8) {
                e.preventDefault();
                return false;
            }
        });
    });
})(jQuery);