/**
 *   Author: Rudz Evan Bernardez, Macquarie University
 *   File Name: ViewModel.js
 *
 */

var viewModel = {
    $text_field: $("#text_field"),
    textAreaStr: ko.observable(""), // For display
    result: ko.observable(""),
    firstIndexOfCurrentWord: 0,
    token: ko.observable(""),
    textList: ko.observableArray([]),
    smallAsp: "",
    bigAsp: "",
    aspState: false,
    allowInput: true,
    catTable: ko.observableArray([]),
    anaExp: ko.observableArray([]),
    lookaheadObject: ko.observableArray([]),
    reasonerMode: "normal", // default settings
    inputMode: ko.observable("Text Mode"), //default settings
    $loader: $(".loader"),
    textParaList: ko.observableArray([]),
    lookUpTable: ko.observableArray([]),
    asp: ko.observable(''),
    answer: ko.observable(''),
    currentInitialLookUpTable: [],
    initSentenceLookUp: [],
    asyncFlag: false,
    fileNames: ko.observableArray([]),
    initialLoad: true,

    init: function () {
        console.log("init should be only called once or after sentence completion")
        var lastNodePostedWasBlank = textLineData.nodes[textLineData.nodes.length - 1] != " ";
        var textAreaEmpty = this.textAreaStr().length == 0;
        if (textAreaEmpty && lastNodePostedWasBlank) {
            this.updateViewForWord(" ");
            this.initSentenceLookUp = this.lookUpTable();
            this.initLookUpObj = this.lookaheadObject();
        }
    },

    loadLookahead: function () {
        if (this.initialLoad) {
            this.init();
            this.initialLoad = false;
        } else {
            this.$text_field.val(this.textAreaStr());
        }
        expressionLoader.loadLookahead();
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
        // If word in lookahead
        var request = $.when(token.postToken(word));
        request.done(function (data) {
            var json = JSON.parse(data);
            if (word == "." || word == "?") {
                viewModel.setAsp(json);
                viewModel.setAnswer(json.answer)
                var para = json.para;
                var newPara = [];
                var s = "";
                for (var z = 0; z < para.length; z++) {
                    if (para[z] == "?" || para[z] == ".") {
                        s = s.slice(0, s.length - 1) + para[z];
                        newPara.push(s)
                        s = "";
                    } else if (para[z] == ",") {
                        s = s.slice(0, s.length - 1) + para[z] + " ";
                    } else {
                        s += para[z] + " ";
                    }
                }
                viewModel.textParaList(newPara);
                viewModel.allowInput = true;
            }

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
    },

    // NAVBAR functions

    // Loads the list of file names in texts folder
    loadFileNames: function () {
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


    // EVENT HANDLER functions

    // Text submitted
    onSubmit: function () {
        eventHandler.enterKey();
    },

    // Char entered
    onKeyPress: function (d, e) {
        return eventHandler.charKey(d, e);
    },

    // Backspace detected
    onKeyUp: function (d, e) {
        var keyVal = e.keyCode;
        if (keyVal == 8) {
            eventHandler.backspace(d, e);
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
