var expressionLoader = {
    loadLookahead: function () {
        var lookaheadTable = lookaheadObj.createLookaheadTable(lookaheadObj);
        viewModel.lookaheadObject(lookaheadTable);
    },

    updateLookUpTable: function () {
        // This concat must be done to avoid unwanted changes in to other data structures
        var tempLookAheadTable = [].concat(viewModel.lookUpTable());
        viewModel.lookUpTable(lookaheadObj.filterTable(viewModel.token(), tempLookAheadTable));
    },

    populateLookUpTable: function (data) {
        lookaheadObj.setAll(data);
        viewModel.lookUpTable(lookaheadObj.getWordTable());
    },

    postLookaheadWord: function (data, event) {
        if (typeof data.add != "undefined") {
            if (confirm('Are you sure you want to add "' + data.add + '" to the temporary vocabulary?')) {
                lookaheadObj.createLookaheadTable(lookaheadObj);
                var catIndex = 0;
                var cat = $(event.target).parent().parent().parent().siblings().html();
                for (i = 0; i < lookaheadObj.cats.length; i++) {
                    if (cat == lookaheadObj.cats[i]) {
                        catIndex = i;
                        break;
                    }
                }
                var num = lookaheadObj.nums[catIndex];
                var vform = (num == " ") ? "bse" : "fin";
                var wform = data.add;
                this.addToLexicon(cat, wform, vform, num);
                textLineData.removeTailNode();
                var dataArr1 = wform.split(" ");
                var str = viewModel.textAreaStr().toString();
                str = str.replace(/^\s+|\s+$/g, "");
                var pos = str.lastIndexOf(" ");
                str = str.substring(0, pos + 1);
                viewModel.textAreaStr(str);
                var s1;
                for (s1 = 0; s1 < dataArr1.length; s1++) {
                    viewModel.updateViewForWord(viewModel.token() + dataArr1[s1]);
                    viewModel.textAreaStr(viewModel.textAreaStr() + dataArr1[s1] + " ");
                    viewModel.firstIndexOfCurrentWord = viewModel.textAreaStr().length;
                    this.loadLookahead();
                    viewModel.$text_field.val(viewModel.textAreaStr());
                }
            } else {
                return false;
            }
        } else if (viewModel.allowInput) {
            var dataArr = data.split(" ");
            var s;
            for (s = 0; s < dataArr.length; s++) {
                viewModel.updateViewForWord(viewModel.token() + dataArr[s]);
                if (dataArr[s] == "." || dataArr[s] == "?") {
                    console.log("full stop");
                    viewModel.isEndOfSentence = true;

                    // The text editor sentence will have space in the end
                    viewModel.textAreaStr(viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length - 1) + dataArr[s] + " ");
                    viewModel.$text_field.val(viewModel.textAreaStr());

                    // viewModel.firstIndexOfCurrentWord = viewModel.textAreaStr().length;
                    viewModel.lookaheadObject(viewModel.initLookUpObj);
                    viewModel.lookUpTable(viewModel.initLookUpTable);
                    
                } else if (data == ",") {
                    viewModel.isEndOfSentence = false;
                    viewModel.textAreaStr(viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length - 1) + dataArr[s] + " ");
                    viewModel.$text_field.val(viewModel.textAreaStr());
                } else {
                    viewModel.isEndOfSentence = false;
                    viewModel.textAreaStr(viewModel.textAreaStr() + dataArr[s] + " ");
                    viewModel.$text_field.val(viewModel.textAreaStr());
                    viewModel.firstIndexOfCurrentWord = viewModel.textAreaStr().length;
                    this.loadLookahead();
                }
            }
            if (!viewModel.allowInput) {
                // viewModel.token(viewModel.token() + dataArr[0]);
                console.log("allowInput false");
            }
        }
    },

    postAnaExpClicked: function (words) {
        var wordA = "" + words;
        var word = wordA.split(" ");
        if (viewModel.lookUpTable().indexOf(word[0]) != -1) {
            if (word.length > 1) {
                for (k = 0; k < word.length; k++) {
                    viewModel.updateViewForWord(word[k]);
                    viewModel.textAreaStr(viewModel.textAreaStr() + word[k] + " ");
                }
            } else {
                viewModel.updateViewForWord(word[0]);
                viewModel.textAreaStr(viewModel.textAreaStr() + word[0] + " ");
            }
            this.loadLookahead();
        } else {
            alert("The anaphoric expression \"" + word + "\" is not a valid token and will not be submitted. " + "Please try another input.");
        }
    },

    addToLexicon: function (cat, wform, vform, num) {
        var idNum = textLineData.nodes.length;
        var fs = this.createFS(cat, wform, vform, num);
        var addData = {
            "id": idNum,
            "inputmode": "text",
            "editmode": "add",
            "token": wform,
            "featurestructure": fs,
            "filename": " ",
            "spectext": " ",
            "snum": textLineData.sentences.length + 1,
            "spos": textLineData.getSpos() - 1,
            "reasoner": "off",
            "reasonermode": "normal"
        }

        $.ajax({
            url: "/peng",
            type: "POST",
            data: addData
        });
    },


    // HELPER functions
    createFS: function (cat, wform, vform, num) {
        var fs = new Object()
        if (cat == "verb: intransitive" || cat == "verb: transitive") {
            fs.cat = cat;
            fs.wform = wform;
            fs.vform = vform;
            fs.num = num;
        }
        else if (cat == "name") {
            fs.cat = cat;
            fs.wform = wform;
            fs.num = num;
        }
        else if (cat == "noun: common" || cat == "noun: relational") {
            fs.cat = cat;
            fs.wform = wform;
            fs.num = num;
        }
        else if (cat == "adjective" || cat == "adjective: relational") {
            fs.cat = cat;
            fs.wform = wform;
        }
        var str = JSON.stringify(fs);
        return str;
    }
}