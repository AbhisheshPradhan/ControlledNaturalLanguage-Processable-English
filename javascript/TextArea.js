
var textLineData = {
  nodes: [],
  sentences: [],
  sposNum: 0,
  firstIndexOfSentence: 0,

  createNode: function(word, rmode) {
      //  var _spos = this.nodes.length-this.firstIndexOfSentence > 1 ? this.nodes.length-this.firstIndexOfSentence-1 : 0;
    var idNum = this.nodes.length+1;    
    var ajaxStruct = {
      id: idNum, //ID starts at 1                          
      inputmode:"text", //inputMode, //"text" ------ FIX
      editmode:"parse", //"parse" ----- FIX
      token: word == "" ? " " : word, // the word itself
      featurestructure:"{ \"cat\" : \" \",  \"wform\" : \" \"}",
      filename:" ", //when loading
      spectext: " ", //
      snum: this.sentences.length+1,
      spos: this.getSpos(), //
      reasoner: (word == "." || word == "?") ? "on" : "off", // on or off
      reasonermode: rmode
    };

    this.addNode(word);

   // this.addNode(word);
    return ajaxStruct;
  },

  addNode: function(node) {
    this.nodes.push(node);
    this.sposNum = node == " " ? 0 : this.sposNum + 1;
  },

  addSentence: function(sentence) {
    this.sentences.push(sentence);
    this.firstIndexOfSentence = this.nodes.length;
  },

  removeTailNode: function() {
    return this.nodes.pop();
  },

  getSpos: function() {
      if(this.nodes.length-this.firstIndexOfSentence > 1)
            return this.nodes.length-this.firstIndexOfSentence-1;
      else
            return 0;
 }
};
