
var textLineData = {
  nodes: [],
  sentences: [],
  sposNum: 0,
  firstIndexOfSentence: 0,

  createNode: function (word, rmode) {
    var idNum = this.nodes.length + 1;
    var ajaxStruct = {
      id: idNum, //ID starts at 1                          
      inputmode: "text",
      editmode: "parse",
      token: word == "" ? " " : word,
      featurestructure: "{ \"cat\" : \" \",  \"wform\" : \" \"}",
      filename: " ",
      spectext: " ",
      snum: this.sentences.length + 1,
      spos: this.sposNum,
      reasoner: (word == "." || word == "?") ? "on" : "off",
      reasonermode: rmode
    };

    this.addNode(word);
    return ajaxStruct;
  },

  addNode: function (node) {
    this.nodes.push(node);
    this.sposNum = node == " " ? 0 : this.sposNum + 1;
  },

  addSentence: function (sentence) {
    this.sposNum = 0;
    this.sentences.push(sentence);
  },

  lastSentenceNodes: function() {
    return this.nodes.lastIndexOf(" ") > this.nodes.lastIndexOf("") ? this.nodes.slice(this.nodes.lastIndexOf(" ")) : this.nodes.slice(this.nodes.lastIndexOf(""));
  },

  removeSentence: function() {
    let lastSentenceNodes = this.lastSentenceNodes();
    this.sposNum = lastSentenceNodes.length - 1;
    return this.sentences.pop();
  },

  removeTailNode: function () {
    this.sposNum = this.sposNum - 1;
    return this.nodes.pop();
  }
};
