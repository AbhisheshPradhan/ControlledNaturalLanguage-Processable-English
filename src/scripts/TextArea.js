
let textLineData = {
  nodes: [],
  sentences: [],
  sposNum: 0,

  createNode: function (word) {
    let idNum = this.nodes.length + 1;
    let ajaxStruct = {
      id: idNum,                    
      inputmode: "text",
      editmode: "parse",
      token: word == "" ? " " : word,
      featurestructure: "{ \"cat\" : \" \",  \"wform\" : \" \"}",
      filename: " ",
      spectext: " ",
      snum: this.sentences.length + 1,
      spos: this.sposNum,
      reasoner: (word == "." || word == "?") ? "on" : "off",
      reasonermode: "normal"
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

  lastSentenceNodes: function () {
    return this.nodes.lastIndexOf(" ") > this.nodes.lastIndexOf("") ? this.nodes.slice(this.nodes.lastIndexOf(" ")) : this.nodes.slice(this.nodes.lastIndexOf(""));
  },

  removeSentence: function () {
    let lastSentenceNodes = this.lastSentenceNodes();
    this.sposNum = lastSentenceNodes.length - 1;
    return this.sentences.pop();
  },

  removeTailNode: function () {
    this.sposNum = this.sposNum - 1 < 0 ? 0 : this.sposNum - 1;
    return this.nodes.pop();
  }
};
