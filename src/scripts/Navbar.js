var navBar = {
    loadFileNames: function () {
        if (viewModel.fileNames().length == 0) {
            var jsonObj = globalHelper.createJsonObject("load", " ", " ", " ", "off", "normal");
            $.ajax({
                url: "/peng",
                type: "POST",
                data: jsonObj,
                success: function (data) {
                    var json = JSON.parse(data);
                    var filenames = json.filenames;
                    viewModel.fileNames(filenames);
                },
                error: function (jqXHR, textStatus, errorThrown) {
                    alert("Failed input on loading file names: \n " + errorThrown);
                }
            });
        }
    },

    saveButton: function () {
        var file_name = prompt("Please enter file name");
        if (file_name != null) {
            viewModel.$loading.show();
            var spectext = "";
            for (var i = 0; i < textLineData.sentences.length; i++) {
                textLineData.sentences[i] = textLineData.sentences[i].split('\r').join('');
                spectext += ('\n' + textLineData.sentences[i] + '\n');
            }

            var saveData = {
                "id": "-1",
                "inputmode": "text",
                "editmode": "save",
                "token": " ",
                "featurestructure": "{ \"cat\" : \" \",  \"wform\" : \" \"}",
                "filename": file_name + ".txt",
                "spectext": spectext,
                "snum": "0",
                "spos": "0",
                "reasoner": "off",
                "reasonermode": "normal"
            }
            $.ajax({
                url: "/peng",
                type: "POST",
                data: saveData,
                success: function () {
                    viewModel.$loading.hide();
                    alert("File saved: " + file_name);
                },
                error: function (jqXHR, textStatus, errorThrown) {
                    viewModel.$loading.hide();
                    alert("Failed to save file: \n " + errorThrown);
                }
            });
        }
    },

    // Load a single file name
    loadFile: function (loadedFileName) {
        let self = this;
        let jsonObj = globalHelper.createJsonObject("load", " ", loadedFileName, " ", "off", "normal");
        
        // if (viewModel.textAreaStr().length == 0 || viewModel.isEndOfSentence) {

        // read the text file and send them as nodes instead..
            if (viewModel.isEndOfSentence || textLineData.nodes[textLineData.nodes.length - 1] == " ") {
            viewModel.$loading.show();
            $.ajax({
                url: "/peng",
                type: "POST",
                data: jsonObj, 
                success: function (data, textStatus, jqXHR) {
                    var json = JSON.parse(data);
                    var nodes = self._formatToReadableInput(json.spectext);
                    viewModel.textList([]);
                    for (i = 0; i < nodes.length; i++) {
                        if (nodes[i] != "") {
                            var x = i;
                            if (nodes[i] == "." || nodes[i] == "?") {
                                viewModel.textAreaStr(viewModel.textAreaStr().slice(0, viewModel.textAreaStr().length - 1) + nodes[i] + " ");
                            } else {
                                viewModel.textAreaStr(viewModel.textAreaStr() + nodes[i] + " ");
                            }
                            viewModel.isLoadFileInput = true;
                            viewModel.$text_field.val(viewModel.textAreaStr());
                            viewModel.updateViewForWord(nodes[i]);
                            i = x;
                        }
                    }
                    viewModel.$loading.hide();
                    // alert("File loaded successfully: ", loadedFileName);
                },
                error: function (jqXHR, textStatus, errorThrown) {
                    viewModel.$loading.hide();
                    alert("Failed JSON object input when loading file: \n " + errorThrown);
                }
            });
        } else {
            alert("Complete sentence before loading file.");
        }

        viewModel.isLoadFileInput = false;

        return true;
    },

    // Generates text
    generateText: function () {
        let self = this;
        this.saveTemporary();
        var jsonObj = globalHelper.createJsonObject("generate", " ", " ", " ", "off", "normal");
        $.ajax({
            url: "/peng",
            type: "POST",
            data: jsonObj,
            success: function (data, textStatus, jqXHR) {
                var json = JSON.parse(data);
                var sentence = "";
                var nodes = self._formatToReadableInput(json.spectext);
                textLineData['sentences'] = [];
                viewModel.textList([]);

                for (i = 0; i < nodes.length; i++) {
                    var x = i;
                    if (nodes[i] == "." || nodes[i] == "?") {
                        sentence = sentence.slice(0, sentence.length - 1);
                        sentence += nodes[i];
                        textLineData.addSentence(sentence);
                        sentence = "";
                    } else {
                        sentence += nodes[i] + " ";
                    }
                    i = x;
                }

                var gsentence = "";
                var gnodes = self._formatToReadableInput(json.gentext);
                for (i = 0; i < gnodes.length; i++) {
                    var x = i;
                    if (gnodes[i] == "." || gnodes[i] == "?") {
                        gsentence = gsentence.slice(0, gsentence.length - 1);
                        gsentence += gnodes[i];
                        viewModel.textList.push(gsentence);
                        gsentence = "";
                    } else {
                        gsentence += gnodes[i] + " ";
                    }
                    i = x;
                }
                viewModel.setAsp(json);
                viewModel.setAnswer(json.answer);
                viewModel.updateViewForWord(" ");
            },
            error: function (jqXHR, textStatus, errorThrown) {
                alert("Failed to generate text: \n " + errorThrown);
            }
        });
        return true;
    },

    saveTemporary: function () {
        var file_name = 'text.tmp';
        if (viewModel.isEndOfSentence || textLineData.nodes[textLineData.nodes.length - 1] == " ") {
            var spectext = "";
            for (var i = 0; i < textLineData.sentences.length; i++) {
                textLineData.sentences[i] = textLineData.sentences[i].split('\r').join('');
                spectext += ('\n' + textLineData.sentences[i] + '\n');
            }

            var saveData = {
                "id": "-1",
                "inputmode": "text",
                "editmode": "save",
                "token": " ",
                "featurestructure": "{ \"cat\" : \" \",  \"wform\" : \" \"}",
                "filename": file_name,
                "spectext": spectext,
                "snum": "0",
                "spos": "0",
                "reasoner": "off",
                "reasonermode": "normal"
            }
            $.ajax({
                url: "/peng",
                type: "POST",
                data: saveData
            });
        } else {
            alert('CNL Text is empty.');
            throw ' ';
        }
    },

    // Helper function
    _formatToReadableInput: function (input) {
        // FORMATS TO READABLE INPUT
        input = input.replace(/%(.)*/g, '');
        input = input.split('\r').join('');
        input = input.split('\n').join('');
        input = input.replace(/\/\*.*\*\//, '');
        input = input.split('\r').join('');
        input = input.split('\n').join('');
        input = input.split('  ').join('');
        input = input.split('.').join(' . ');
        input = input.split('?').join(' ? ');
        input = input.split(', ').join(' , ');
        console.log("input", input);
        input = input.split(" ");
        input.pop();
        return input;
    }
}