class Translator

  class Word
    # Word class
    attr_accessor :word, :pos, :translations

    def initialize(word, pos, translations)
      @word = word  # => "blue"
      @pos = pos  # => "ADJ"
      @translations = translations  # => {"L1"=>"bleu", "L2"=>"blau", "L3"=>"azul"}
    end
  end

  class Lexicon
    # A lexicon is a hash for ADJ, NOU, DET, PRE
    # { POS => words, POS => words, POS => words, POS => words }
    # 
    # Where each "words" will have: 
    # { word => translations, word => translations, ... }
    # 
    # Where each translation is:
    # {"L1"=>"bleu", "L2"=>"blau", "L3"=>"azul"}
    #
    # => { "ADJ" => { "word" => translations , ... } , "NOU" => {  }, "DET" => {  }, "PRE" => {  } }

    def initialize
      @lexicon = Hash.new
    end

    def addWord(word)
      @lexicon[word.pos] = Hash.new if @lexicon[word.pos].nil?
      @lexicon[word.pos][word.word] = word.translations
      # => { "ADJ" => { "word" => translations , ... } , "NOU" => {  }, "DET" => {  }, "PRE" => {  } }
    end

    def getLexicon
      @lexicon
    end

    # getWords("ADJ")
    def getWords(pos)
      @lexicon[pos]
      # => { "word" => translations , "word2" => translations , ... }
    end

    # getWordsByLanguageAndPos("Spanish", "ADJ")
    def getWordsByLanguageAndPos(language, pos)
      selected_words = Hash.new # => {  }

      # Edge Case
      if @lexicon[pos].nil?
        return nil
      end
      
      #if english, get all words from POS 
      if language == "English"
        selected_words = @lexicon[pos]
      else 
        # Other than english, get words with their language translations   
        selected_words = @lexicon[pos].select { |word, translations| translations.has_key?(language) }
      end

      return selected_words
      # => {"blue"=>translation , "red"=>translation , ...}
      # translation has "Spanish"
      # this only is ADJ
    end
  end # endofclass Lexicon

  
  # Class Grammar
  class Grammar
    attr_accessor :rules
    # A grammar has a hash of rules
    # { language => [POS,POS,POS,...] , language => [POS,POS,POS,...] , ... }
    
    def initialize
      @rules = Hash.new
    end

    # Add rule
    def addRule(language, pos)
      @rules[language] = pos
    end

    # Get rule by language or POS
    def getRuleByLanguageOrPOS(language)
      # if language is a string
      if language.is_a?(String)
        return @rules[language]
      # if language is an array
      elsif language.is_a?(Array)
        return language
      end
    end
  end # endofclass

  # Translator constructor
  def initialize(words_file, grammar_file)
    @lexicon = Lexicon.new
    @grammar = Grammar.new
    updateLexicon(words_file)
    updateGrammar(grammar_file)
  end

  # part 1
  def updateLexicon(inputfile)
    # Open file
    file = File.open(inputfile, "r")
    
    # Read file
    file.each_line do |line|

      line = line.chomp.strip # Remove trailing newline, leading & trailing whitespaces
      
      # Pattern match for: (word) , (POS) , (<L1>:<word1>, <L2>:<word2>, ...)
      if line =~ /^([a-z-]+),\s([A-Z]{3}),\s(.*)/

        # Get word
        word = $1.strip
      
        # Get PartOfSpeech
        pos = $2.strip
        
        # Get all translations
        translations = $3 # "L1:word1, L2:word2, L3:word3"
        translations = translations.chomp.strip # Remove leading & trailing whitespaces
        
        #edge case
        next if !(translations =~ /^([A-Z][a-z0-9]*:[a-z-]+(, )?)+$/)
        #edge case /([A-Z][a-z0-9]+:[a-z-]+)/


        # Split translations into (language:word) pairs
        translations = translations.split(/\s*,\s*/)  # =>["L1:word1", "L2:word2"]

        # Create hash of languages and words
        translations = Hash[translations.map { |translation| translation.split(/:/) }]
        # .map: ["L1:word1", "L2:word2"] => [ ["L1", "word1"], ["L2", "word2"] ] 
        # Hash: Hash[ [["L1", "word1"], ["L2", "word2"]] ] 
        # => {"L1"=>"word1", "L2"=>"word2"}

        # Create word object
        word = Word.new(word, pos, translations)

        # Add word to lexicon
        @lexicon.addWord(word)
        # If a word is already in the POS, update the translations
        # If a word is already in Lexicon but different POS, add word to its POS
      end
    end
    file.close
  # endofdef
  end

  # 1. Update Grammar
  def updateGrammar(inputfile)
    flag = false

    # Open file
    file = File.open(inputfile, "r")

    # Read file
    file.each_line do |line|
      new_pos = Array.new

      # Get rid of whitespaces and new lines
      line = line.strip.chomp

      # Pattern match for: (Language): (POS, POS, POS,...)
      if line =~ /^([A-Z][a-z0-9]*):\s(.*)/

        # Get language
        language = $1

        # Get all POS
        pos = $2 # "POS, POS, POS"

        # Check "POS, POS, POS, ..." is valid
        next if !(pos =~ /^([A-Z]{3}({[0-9]+})?(, )?)+$/ )
        # Split POS into array
        pos = pos.split(/\s*,\s*/) # => ["POS", "POS", "POS"]
        
        info = /([A-Z]{3}){([0-9]+)}/

        pos.each do |e|
          if e =~ info
            for i in 0...($2.to_i)
              new_pos.push($1)
            end
          else
            new_pos.push(e)
          end
        end

        # Add rule to grammar
        @grammar.addRule(language, new_pos)

      end
    end#endoffile
    file.close
  
  end#endofdef


  # part 2
  # 2. Generate Sentence
  # ("English", "French")
  # ("English", ["ADJ", "NOU", "DET"])
  def generateSentence(language, struct)
    sentence = ""
    rule = @grammar.getRuleByLanguageOrPOS(struct)

    # Edge case
    if rule.nil?
      return nil
    end

    # ["NOU", "AJD", "JKJ"]
    rule.each do |pos|
      languageWords = Array.new

      words = @lexicon.getWordsByLanguageAndPos(language, pos)
      #=> {"blue"=>translation , "red"=>translation , ...}

      # Edge case
      if words.nil?
        return nil
      end

      # Edge case
      # If generate English sentence
      if language == "English"
        sentence += words.keys.sample + " "
        next
      end

      # If generate other than English 
      # Pick all words by a specific language
      for word, translation in words
        for k, v in translation
          if k == language
            languageWords.push(v)
          end
        end
      end
      
      # Edge case
      if languageWords.empty?
        return nil
      elsif languageWords.sample.nil?
        return nil
      end
  
      # Get random word of a specific language      
      sentence += languageWords.sample + " "
    end

    sentence = sentence.strip
    
    return sentence
  end


  # 3. Check Grammar
  # Check if sentence is grammatically correct
  def checkGrammar(sentence, language)
    
    # Get rule by language
    rule = @grammar.getRuleByLanguageOrPOS(language)
    # ["DET", "ADJ", "NOU"]


    # Edge case
    if rule.nil?
      return false
    end
    
    # ["the", "blue", "fork"]
    sentence = sentence.chomp.strip.split(/\s+/)

    i = 0
    # sent: ["the", "blue", "fork"]
    # rule: ["DET", "ADJ", "NOU"]
    for eachPOS in rule
      flag = false

      # if the sentence[i] is English
      if @lexicon.getLexicon[eachPOS].has_key?(sentence[i])
        flag = true
        i += 1
        next
      else # if the sentence[i] is not English
        @lexicon.getLexicon[eachPOS].each do |word, translations|
          if translations.has_value?(sentence[i])
            flag = true
            i += 1
            break
          end
        end
      end

      if flag == false
        return false
      end
    end      
  end


  # 4. Change Grammar
  def changeGrammar(sentence, struct1, struct2)

    # "w-onesix w-onesix w-onethree w-twosix"
    sentence = sentence.chomp.strip

    sentence_change_arr = Array.new

    # get rule for struct1
    rule1 = @grammar.getRuleByLanguageOrPOS(struct1)

    # get rule for struct2
    rule2 = @grammar.getRuleByLanguageOrPOS(struct2)

    if !((rule1.uniq - rule2.uniq).empty?)
      return nil
    end

    # Get array of the sentence
    sentence_array = sentence.split(/\s+/)

    # Combine rule1 and sentence_array in pairs
    zipped = rule1.zip(sentence_array)  # => [["ADJ", "blue"], ["DET", "the"], ["NOU", "truck"]]
    
    # loop thru rule2
    # for each of rule2
    # get the key and find the value in the zipped
    # add the value to the sentence_changed

    # zipped: [ ["JKJ","w-onesix"], ["JKJ","w-onesix"], ["AJD","w-onethree"], ["NOU","w-twosix"] ]
    # rule2: [ "JKJ", "JKJ", "NOU", "AJD"]
    for each in rule2
      found = false
      for key, value in zipped
        if key == each
          found = true
          
          sentence_change_arr.push(value)

          # delete the index of key and value in zipped
          zipped.delete_at(zipped.index([key, value]))
          break
        end
      end

      if found == false
        return nil
      end
    end

    sentence_changed = sentence_change_arr.map{ |i| i}.join(" ")
    
    sentence_changed = sentence_changed.strip

    return sentence_changed
  end


  # part 3
  # 5. Change Language
  def changeLanguage(sentence, language1, language2)
    sentence = sentence.chomp.strip
    
    language_changed = ""

    # Get rule for language1
    rule1 = @grammar.getRuleByLanguageOrPOS(language1)

    # Get rule for language2
    rule2 = @grammar.getRuleByLanguageOrPOS(language2)

    # Edge case
    if rule1.nil? or rule2.nil?
      return nil
    end

    # Get array of the sentence
    sentence_array = sentence.split(/\s+/)

    i = 0
    # If language1 is English
    if language1 == "English"
      # at each rule[i]
      for eachPOS in rule1
        found = false
        hashWords = @lexicon.getWords(eachPOS)


        for word,translation in hashWords
          if word == sentence_array[i] and translation.has_key?(language2) 
            found = true
            translated = translation[language2]
            if translated.nil? or translated.empty?
              return nil
            end
            language_changed += translated + " "
            break
          end
        end

        if found == false
          return nil
        end
        i = i + 1
      end

    elsif language2 == "English"  # If language2 is English
      # at each rule[i]
      for eachPOS in rule1
        found = false
        hashWords = @lexicon.getWords(eachPOS)

        for word,translation in hashWords
          if translation.has_key?(language1) and translation[language1] == sentence_array[i]
            found = true
            language_changed += word + " "
            break
          end
        end
        
        if found == false
          return nil
        end
        i = i + 1
      end
    
    else # if language1 and language2 are not English
      # at each rule[i]
      for eachPOS in rule1
        found = false
        hashWords = @lexicon.getWords(eachPOS)

        for word,translation in hashWords
          if translation.has_key?(language1) and translation.has_key?(language2) and translation[language1] == sentence_array[i] 
            found = true
            # what if there is no translation for language2?
            translated = translation[language2]
            if translated.nil? or translated.empty?
              return nil
            end
            language_changed += translation[language2] + " "
            break
          end
        end
        
        if found == false
          return nil
        end
        i = i + 1
      end
    end
        
    language_changed = language_changed.strip
    return language_changed
  end

  # 6. Translate
  def translate(sentence, language1, language2)
    result = changeLanguage(sentence, language1, language2)

    if result == nil
      return nil
    end

    result = changeGrammar(result, language1, language2)
    if result == nil
      return nil
    end

    return result
  end

end