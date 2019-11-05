/**
*  Lookahead object, modified by Rolf Schwitter
*/
let lookaheadObj = {
  wordTable: [],    // table for words for current token
  cats: [], //categories
  wforms: [],
  vforms: [],
  nums: [],
  catsCanAdd: ["verb: intransitive", "verb: transitive", "name", "noun: common", "noun: relational", "adjective", "adjective: relational"],

  getWordTable: function () {
    return this.wordTable;
  },

  emptyAll: function () {
    this.wordTable = [];
    this.cats = [];
    this.wforms = [];
    this.vforms = [];
    this.nums = [];
  },

  // sets cats, wform, vform, num into their own list, assuming they exist
  setAll: function (data) {
    this.emptyAll();
    if (data.hasOwnProperty('lookahead')) {
      let numOfCat = data.lookahead.length;  //number of categories available
      for (i = 0; i < numOfCat; i++) {
        this.cats.push(data.lookahead[i].cat);
        this.wforms.push(data.lookahead[i].wform);
        if (data.lookahead[i].hasOwnProperty('vform')) {
          this.vforms.push(data.lookahead[i].vform);
        } else {
          this.vforms.push(" ");
        }
        if (data.lookahead[i].hasOwnProperty('num')) {
          this.nums.push(data.lookahead[i].num);
        } else {
          this.nums.push(" ");
        }
      }

      let temp = [];
      for (i = 0; i < this.wforms.length; i++) {
        for (j = 0; j < this.wforms[i].length; j++) {
          temp.push(this.wforms[i][j][0]);
        }
      }
      //remove duplicates of word table
      let temp2 = [];
      $.each(temp, function (i, el) {
        if ($.inArray(el, temp2) === -1) temp2.push(el);
      });
      this.wordTable = temp2;
    }
  },

  // Removes all values in a lookahead table that does not correspond with token  (@ a node level).
  filterTable: function (token, lookahead) {
    for (let i = 0; i < lookahead.length; i++) { //CHANGE TO FILTER
      let notSameString = token != lookahead[i].slice(0, token.length);
      if (notSameString) {
        lookahead.splice(i, 1);
        i--;
      }
    }
    return lookahead;
  },

	/**
	*     lookahead: JSON object from PENG server
	*     return: an array containing lookahead data containing {cat, wform}
	*/
  createLookaheadTable: function (lookahead) {
    let wform = [];
    let temp = [];
    for (i = 0; i < lookahead.cats.length; i++) {
      let wform = [];
      for (j = 0; j < lookahead.wforms[i].length; j++) {
        let word = lookahead.wforms[i][j].join(" ");
        wform.push(word);
      }
      temp.push({ "cat": lookahead.cats[i], "wform": wform });
    }
    return temp;
  },

	/**
	*     lookahead: JSON object from PENG server
	*     return: an array containing lookahead data containing {cat, wform}
	*/
  createCatTable: function (lookahead) {
    let wform = [];
    let temp = [];
    for (i = 0; i < lookahead.cats.length; i++) {
      let wform = [];
      for (j = 0; j < lookahead.wforms[i].length; j++)
        wform.push(lookahead.wforms[i][j][0]);
      temp.push(wform);
    }
    return temp;
  },

	/**
	* Return: an array with str inserted in the head of wform
	*/
  addStrInHeadForEachCatInLookahead: function (str, lookaheadObject) {
    // Add modal here
    alert('"' + str + '" is not a valid word. Please go back and re-enter a word or add the word to the lexicon via the Lookahead Information menu.');
    let catArr = ["verb: intransitive", "verb: transitive", "name", "noun: common", "noun: relational", "adjective", "adjective: relational"];
    for (i = 0; i < lookaheadObject.length; i++) {
      if (catArr.includes(lookaheadObject[i].cat)) {
        lookaheadObject[i].wform.unshift({ "add": str });
      }
    }
    return lookaheadObject;
  }
}
