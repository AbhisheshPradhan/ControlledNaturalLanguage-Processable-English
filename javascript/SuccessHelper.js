/**
*     Contains helper functions for when AJAX requests is successful
*     Modified by Rolf Schwitter: 2019-01-04
*/


var SuccessHelper = {

      loadSingleTextFile: function(data, textStatus, jqXHR) {
            var r = "";
            var json = JSON.parse(data);
            var s = json.spectext;
            var sentence = "";
            // FORMATS TO READABLE INPUT
            s = s.replace(/%(.)*/g, '');
            s = s.split('\r').join('');
            s = s.split('\n').join('');
            s = s.replace(/\/\*.*\*\//, '');
            s = s.split('\r').join('');
            s = s.split('\n').join('');
            s = s.split('  ').join('');
            s = s.split('.').join(' . ');
            s = s.split('?').join(' ? ');
            s = s.split(', ').join(' , ');
            

            var nodes = s.split(" ");
            
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
                    sentence+=nodes[i]+" "; }
                i = x;
            }
	      //viewModel.anaExp(json.ana);
            viewModel.setAsp(json);
            viewModel.setAnswer(json.answer);
            viewModel.postToken(" ");
            viewModel.$loader.css("visibility", "hidden");
      },

      generateText: function(data, textStatus, jqXHR) {
            var r = "";
            var json = JSON.parse(data);
            var s = json.spectext;
            var sentence = "";
            // FORMATS TO READABLE INPUT
            s = s.replace(/%(.)*/g, '');
            s = s.split('\r').join('');
            s = s.split('\n').join('');
            s = s.replace(/\/\*.*\*\//, '');
            s = s.split('\r').join('');
            s = s.split('\n').join('');
            s = s.split('  ').join('');
            s = s.split('.').join(' . ');
            s = s.split('?').join(' ? ');
            s = s.split(', ').join(' , ');

            var nodes = s.split(" ");
            
            textLineData['sentences'] = [];
	      viewModel.textList([]);

            for (i = 0; i < nodes.length; i++) {
                 var x = i;
                 if (nodes[i] == "." || nodes[i] == "?") {
                        sentence = sentence.slice(0, sentence.length - 1);
                        sentence += nodes[i];
                        textLineData.addSentence(sentence);
                        // viewModel.textList.push(sentence);
                        sentence = "";
                  }
                else {
                    sentence+=nodes[i]+" "; }
                i = x;
            }

            var g = json.gentext;
            var gsentence = "";
            // FORMATS TO READABLE INPUT
            g = g.replace(/%(.)*/g, '');
            g = g.split('\r').join('');
            g = g.split('\n').join('');
            g = g.replace(/\/\*.*\*\//, '');
            g = g.split('\r').join('');
            s = g.split('\n').join('');
            g = g.split('  ').join('');
            g = g.split('.').join(' . ');
            g = g.split('?').join(' ? ');
            g = g.split(', ').join(' , ');
            

            var gnodes = g.split(" ");
            
            //textLineData['sentences'] = [];
	      //viewModel.textList([]);

            for (i = 0; i < gnodes.length; i++) {
                 var x = i;
                 if (gnodes[i] == "." || gnodes[i] == "?") {
                        gsentence = gsentence.slice(0, gsentence.length - 1);
                        gsentence += gnodes[i];
                        // textLineData.addSentence(sentence);
                        viewModel.textList.push(gsentence);
                        gsentence = "";
                  }
                else {
                    gsentence+=gnodes[i]+" "; }
                i = x;
            }  
	      //viewModel.anaExp(json.ana);
            viewModel.setAsp(json);
            viewModel.setAnswer(json.answer);
            viewModel.postToken(" ");
            viewModel.$loader.css("visibility", "hidden");
	    
      },


      postTokenForLoading: function(data) {
                  var json = JSON.parse(data);
                  viewModel.populateLookUpTable(json);

                  if (word == "." || word == "?") {
                        viewModel.setAsp(json.asp);
                        viewModel.allowInput = true;
                  }

                  if(json.hasOwnProperty('spelling suggestions') || (json.lookahead.length == 0 && !json.hasOwnProperty('asp'))) {
                        viewModel.allowInput = false;
                        alert('"'+word+'" is not a valid token. Please go back and re-enter a token or add to lexicon');
                        viewModel.addButton("Add");
                  }
                  else {
                        viewModel.allowInput = true;
                  }

                  if(json.hasOwnProperty('ana') && word != ".") {
                        viewModel.anaExp(json.ana);
                  }
      }
}
