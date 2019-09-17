
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
      spos: this.getSpos(),
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
    this.sentences.push(sentence);
    this.firstIndexOfSentence = this.nodes.length;
  },

  removeTailNode: function () {
    return this.nodes.pop();
  },

  getSpos: function () {
    if (this.nodes.length - this.firstIndexOfSentence > 1) {
      return this.nodes.length - this.firstIndexOfSentence - 1;
    }
    return 0;
  }
};
