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
        var file_name = prompt("Please enter your name");
        if (file_name != null) {
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
        }
    },

    // Load a single file name
    loadFile: function (loadedFileName) {
        let self = this;
        let jsonObj = globalHelper.createJsonObject("load", " ", loadedFileName, " ", "off", "normal");
        $.ajax({
            url: "/peng",
            type: "POST",
            data: jsonObj,
            success: function (data, textStatus, jqXHR) {
                var json = JSON.parse(data);
                var nodes = self._formatToReadableInput(json.spectext);
                var sentence = "";
                textLineData['sentences'] = [];
                viewModel.textList([]);
                for (i = 0; i < nodes.length; i++) {
                    var x = i;
                    if (nodes[i] == "." || nodes[i] == "?") {
                        sentence = sentence.slice(0, sentence.length - 1);
                        sentence += nodes[i];
                        textLineData.addSentence(sentence);
                        viewModel.textList.push(sentence);
                        sentence = "";
                    }
                    else {
                        sentence += nodes[i] + " ";
                    }
                    i = x;
                }
                viewModel.setAsp(json);
                viewModel.setAnswer(json.answer);
                viewModel.updateViewForWord(" ");
                viewModel.$loader.css("visibility", "hidden");
            },
            error: function (jqXHR, textStatus, errorThrown) {
                alert("Failed JSON object input when loading file: \n " + errorThrown);
            }
        });
        return true;
    },

    // Generates text
    generateText: function () {
        let self = this;
        saveTemporary();
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
                viewModel.$loader.css("visibility", "hidden");
            },
            error: function (jqXHR, textStatus, errorThrown) {
                alert("Failed to generate text: \n " + errorThrown);
            }
        });
        return true;
    },

    saveTemporary: function () {
        var file_name = 'text.tmp';
        if (viewModel.textLineData.sentences.length > 0) {
            var spectext = "";
            for (var i = 0; i < viewModel.textLineData.sentences.length; i++) {
                viewModel.textLineData.sentences[i] = viewModel.textLineData.sentences[i].split('\r').join('');
                spectext += ('\n' + viewModel.textLineData.sentences[i] + '\n');
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
        return input.split(" ");
    }
}