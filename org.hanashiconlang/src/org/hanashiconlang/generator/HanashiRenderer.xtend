package org.hanashiconlang.generator

import java.util.ArrayList
import java.util.HashMap
import java.util.function.Function
import java.util.regex.Pattern
import org.eclipse.emf.common.util.EList
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.util.Pair
import org.eclipse.xtext.util.Tuples
import org.hanashiconlang.hanashi.Article
import org.hanashiconlang.hanashi.Call
import org.hanashiconlang.hanashi.Footnote
import org.hanashiconlang.hanashi.Gloss
import org.hanashiconlang.hanashi.GlossMorpheme
import org.hanashiconlang.hanashi.GlossRichString
import org.hanashiconlang.hanashi.GlossSkip
import org.hanashiconlang.hanashi.GlossString
import org.hanashiconlang.hanashi.GlossWord
import org.hanashiconlang.hanashi.Language
import org.hanashiconlang.hanashi.Lexicon
import org.hanashiconlang.hanashi.Morpheme
import org.hanashiconlang.hanashi.PartLexicon
import org.hanashiconlang.hanashi.PartSection
import org.hanashiconlang.hanashi.PartTOC
import org.hanashiconlang.hanashi.PartTaxonomy
import org.hanashiconlang.hanashi.RichString
import org.hanashiconlang.hanashi.RichStringLiteral
import org.hanashiconlang.hanashi.Section
import org.hanashiconlang.hanashi.Syntax
import org.hanashiconlang.hanashi.Table
import org.hanashiconlang.hanashi.TableCol
import org.hanashiconlang.hanashi.TableRow
import org.hanashiconlang.hanashi.Taxon
import org.hanashiconlang.hanashi.Translated

import static extension org.hanashiconlang.HanashiExtensions.*
import org.hanashiconlang.hanashi.InlineGloss

class HanashiRenderer {
	var Language lang
	val postFuncs = new ArrayList<Function<CharSequence, CharSequence>>
	var footnoteHumanCounter = 1
	var footnoteGlobalCounter = 0
	val footnoteHash = new HashMap<String, Pair<Integer, Integer>>()
	
	new(Language lang) {
		this.lang = lang
	}
	
	def CharSequence formString(Morpheme l) {
		richString2String(l.entries?.findFirst[type=="form"]?.text) ?: l.name
	}
    def Iterable<CharSequence> formStrings(Morpheme l) {
        val forms = l.entries?.
            filter[type == "form"]?.
            map[richString2String(it.text)]
        if (forms.nullOrEmpty) #[l.name]
        else forms
    }
	def CharSequence glossString(Morpheme l) {
		richString2String(l.entries?.findFirst[ 
			type == "gloss" && language == lang
		]?.text) ?: ""
	}
	def CharSequence translationString(Morpheme l) {
		richString2String(l.entries?.findFirst[ 
			type == "translation" && language == lang
		]?.text) ?: ""
	}
	
	def CharSequence title(Section s) {
	    if (s.title !== null) generateRichString(s.title, false)
	    else s.name
	}
    def CharSequence title(Taxon s) {
        if (s.title !== null) generateRichString(s.title, false)
        else "Taxonomy " + s.name
    }
    def CharSequence title(Lexicon s) {
        if (s.title !== null) generateRichString(s.title, false)
        else "Lexicon " + s.name
    }
    def CharSequence title(Morpheme s) {
        if (s.title !== null) generateRichString(s.title, false)
        else s.name
    }
    def CharSequence title(Syntax s) {
        if (s.title !== null) generateRichString(s.title, false)
        else s.name
    }
    def dispatch hasName(Section s) {
        s.name !== null
    }
    def dispatch hasName(Taxon t) {
        t.name !== null
    }
    def dispatch hasName(Lexicon l) {
        l.name !== null
    }
    def dispatch hasName(Morpheme m) {
        m.name !== null
    }
    def dispatch hasName(Syntax s) {
        s.name !== null
    }
    def dispatch hasName(PartSection s) {
        s.target.name !== null
    }
    def dispatch hasName(PartTaxonomy t) {
        t.target.name !== null
    }
    def dispatch hasName(PartLexicon l) {
        l.target.name !== null
    }

	def CharSequence generateRichString(RichString s, boolean requirePara) {
		if (s !== null) {
			val texts = s.expressions.indexed.map[
				generateRichStringExpression(it.value, it.key == 0, it.key == s.expressions.size - 1)
			].toList
			val text = texts.map[first].join
			if (requirePara || texts.exists[it.second])
					'''<p>«text»</p>'''
				else
					text
		} else 
			null
	}
	val multiNewlines = Pattern.compile("\\r?\\n([ \\t]*\\r?\\n)+")
	dispatch def generateRichStringExpression(RichStringLiteral l, boolean trimLeft, boolean trimRight) {
		val v = l.value
		val v2 = if (trimLeft) { 
			var i = 0
			while (i < v.length && v.charAt(i) <= 0x20) i += 1;
			v.substring(i)
			} else v
		val v3 = if (trimRight) { 
			var i = v2.length
			while (i > 0 && v2.charAt(i - 1) <= 0x20) i -= 1;
			v2.substring(0, i)
			} else v2
		val m = multiNewlines.matcher(v3)
		val found = m.find
		Tuples.create(m.replaceAll("</p>\n<p>"), found)
	}
	dispatch def generateRichStringExpression(InlineGloss g, boolean trimLeft, boolean trimRight) {
 	    val classes = #["inline-gloss"] + (richString2String(g.html.class_)?.toString?.split(" ") ?: newArrayOfSize(0))
        Tuples.create('''<ruby class="«classes.join(" ")»">«
            generateGlossWordsText(g.line.words, g.line.words.size, true)»«
            if (g.line.words.exists[items.exists[it instanceof GlossMorpheme]])
                generateGlossWordsInfo(g.line.words, g.line.words.size, true)
            »</ruby>''', false)
    }
	dispatch def generateRichStringExpression(Gloss g, boolean trimLeft, boolean trimRight) { 
		val classes = #["gloss"] + (richString2String(g.html.class_)?.toString?.split(" ") ?: newArrayOfSize(0))
		val numCols = g.lines.map[words.size].max
		Tuples.create('''
			<table class="«classes.join(" ")»">
			«FOR l: g.lines»
    		    <tr class="gloss-words">«generateGlossWordsText(l.words, numCols, false)»</tr>«
                IF (l.words.exists[items.exists[it instanceof GlossMorpheme]])
	                »<tr class="gloss-info">«
			    		generateGlossWordsInfo(l.words, numCols, false)
		    		»</tr>«
                ENDIF»
		    «ENDFOR»
			</table>''', false)
	}
	dispatch def generateRichStringExpression(Call c, boolean trimLeft, boolean trimRight) {
	    val func = richString2String(c.function).toString
	    val field = HanashiFunctions.getDeclaredField(func)
	    Tuples.create((field.get(null) as HanashiFunction).generate(
	        c.arguments, this
	    ), false)
	}
	dispatch def generateRichStringExpression(Table t, boolean trimLeft, boolean trimRight) {
		val classes = #["table"] + (richString2String(t.html.class_)?.toString?.split(" ") ?: newArrayOfSize(0))
		Tuples.create('''
		<table class="«classes.join(" ")»">
			«FOR r: t.rows»«generateTableRow(r)»«ENDFOR»
		</table>''', false)
	}
	def generateTableRow(TableRow r) 
		'''
		<tr>«FOR c: r.cols»«generateTableCol(c)»«ENDFOR»<tr/>
		'''
	def generateTableCol(TableCol c) {
		val text = generateRichString(c.text, false)
		val colspan = if (c.skips.nullOrEmpty) ""
			else ''' colspan="«c.skips.size + 1»"'''
		'''«IF c.header»<th«colspan»>«text»</th>«ELSE»<td«colspan»>«text»</td>«ENDIF»'''
	}
	dispatch def generateRichStringExpression(Footnote f, boolean trimLeft, boolean trimRight) {
		val name = if (f.target !== null) f.target.name else f.name
		val counters = if (name === null || !footnoteHash.containsKey(name)) {
				val counters = Tuples.create(footnoteHumanCounter, footnoteGlobalCounter)
				if (name !== null)
					footnoteHash.put(name, counters)
				footnoteHumanCounter += 1
				footnoteGlobalCounter += 1
				counters
			} else 
				footnoteHash.get(name)
		if (f.target === null)
			postFuncs.add[
				'''«it»<p id="#footnote-«counters.second»"><sup>«counters.first»</sup> «generateRichString(f.text, false)»</p>'''
			]
		Tuples.create('''<a href="#footnote-«counters.second»"><sup>«counters.first»</sup></a>''', false)
	}
	def generatePostFuncs(CharSequence text) {
		var result = text
		while (!postFuncs.empty) {
			result = postFuncs.remove(0).apply(result)
		}
		footnoteHumanCounter = 1
		result
	}
	dispatch def generateRichStringExpression(Translated t, boolean trimLeft, boolean trimRight) { 
		Tuples.create(
			'''«FOR i: t.items.filter[it.language == this.lang]»«generateRichString(i.text, false)»«ENDFOR»''',
			false
		)
	}
	
	def CharSequence richString2String(RichString s) {
		if (s !== null)
			'''«FOR e: s.expressions»«richStringExpression2String(e)»«ENDFOR»'''
		else
			null
	}
	dispatch def richStringExpression2String(RichStringLiteral l) {
		l.value
	}
	dispatch def richStringExpression2String(Gloss g) {
		glossWords2String(g.lines.get(0).words)
	}
    dispatch def CharSequence richStringExpression2String(Call c) {
        val func = richString2String(c.function).toString
        val field = HanashiFunctions.getDeclaredField(func)
        (field.get(null) as HanashiFunction).string(
            c.arguments, this
        )
    }
    dispatch def CharSequence richStringExpression2String(Table t) {
    	""
    }
    dispatch def CharSequence richStringExpression2String(Footnote f) {
    	""
    }
	dispatch def richStringExpression2String(Translated t)
		'''«FOR i: t.items.filter[it.language == this.lang]»«richString2String(i.text)»«ENDFOR»'''
	
	def generateGlossWordsText(EList<GlossWord> gws, int numCols, boolean ruby) {
	    val pure = !gws.exists[it.items.exists[it instanceof GlossMorpheme]]
	    var col = 0 
		'''«FOR gw: gws.indexed SEPARATOR if (ruby) "<rb> </rb>"»«
		  IF ruby»<rb>«ELSE»<td «
			  IF gw.key == gws.size - 1 && col != numCols - 1»colspan="«numCols-col»"«
			  ELSEIF gw.value.skips.size > 0»colspan="«gw.value.skips.size + 1»"«ENDIF»>«ENDIF»«
		  IF pure && gw.key == 0»“«ENDIF»«
		  FOR gi: gw.value.items.indexed»«
		      IF gi.key > 0 && gi.value instanceof GlossMorpheme && 
		              gw.value.items.get(gi.key - 1) instanceof GlossMorpheme»«ENDIF»«
              generateGlossText(gi.value)»«
          ENDFOR
          »«{col += gw.value.skips.size + 1; null}
          »«IF pure && gw.key == gws.size - 1»”«ENDIF»«IF ruby»</rb>«ELSE»</td>«ENDIF»«ENDFOR»'''
    }
	dispatch def generateGlossText(GlossMorpheme gl) { 
		val classes = #["morpheme-ref"] + 
		    (richString2String(gl.morpheme.html.refclass)?.toString?.split(" ") ?: newArrayOfSize(0)) + 
			(richString2String(gl.morpheme.lexicon.morphemerefclass)?.toString?.split(" ") ?: newArrayOfSize(0))
		'''<a href="#morpheme$«gl.morpheme.language.name»$«gl.morpheme.name»" title="«gl.morpheme.translationString»" classes="«classes.join(" ")»">«gl.morpheme.formString»</a>'''
	}
	dispatch def generateGlossText(GlossString gs) {
		gs.string
	} 
	dispatch def generateGlossText(GlossRichString gs) {
		generateRichString(gs.string, false)
	} 
	def generateGlossWordsInfo(EList<GlossWord> gws, int numCols, boolean ruby) {
		var col = 0 
		'''«IF ruby»<rtc class="gloss-info">«ENDIF»«
		FOR gw: gws.indexed SEPARATOR if (ruby) "<rt></rt>"»«IF ruby»<rt>«ELSE»<td«
		    IF gw.key == gws.size - 1 && col != numCols - 1» colspan="«numCols-col»"«
		    ELSEIF gw.value.skips.size > 0» colspan="«gw.value.skips.size + 1»"«ENDIF»>«ENDIF»«
	    	FOR gi: gw.value.items.indexed»«
                IF gi.key > 0 && gi.value instanceof GlossMorpheme && 
                        gw.value.items.get(gi.key - 1) instanceof GlossMorpheme».«ENDIF»«
                generateGlossInfo(gi.value)»«ENDFOR»«IF ruby»</rt>«ELSE»</td>«ENDIF»«
        	{col += gw.value.skips.size + 1; null}»«
        ENDFOR»«
        IF ruby»</rtc>«ENDIF»'''
    }
	dispatch def generateGlossInfo(GlossMorpheme gl) 
		'''«gl.morpheme.glossString»'''
	dispatch def generateGlossInfo(GlossString gs) {
		gs.string
	}
	dispatch def generateGlossInfo(GlossRichString gs) {
		richString2String(gs.string)
	}
	def glossWords2String(EList<GlossWord> gws)
		'''«FOR gw: gws SEPARATOR " "»«FOR gi: gw.items»«glossItem2String(gi)»«ENDFOR»«ENDFOR»'''
	dispatch def glossItem2String(GlossMorpheme gm) {
		gm.morpheme.glossString
	}
	dispatch def glossItem2String(GlossString gs) {
		gs.string
	}
	dispatch def glossItem2String(GlossRichString gs) {
		richString2String(gs.string)
	}
	dispatch def glossItem2String(GlossSkip gs) {
		''
	}
	
	dispatch def CharSequence generatePart(PartSection p, int depth) {
	    generateDeclaration(p.target, depth)
	}
    dispatch def CharSequence generatePart(PartLexicon p, int depth) {
        generateDeclaration(p.target, depth)
    }
    dispatch def CharSequence generatePart(PartTaxonomy p, int depth) {
        generateDeclaration(p.target, depth)
    }
    dispatch def CharSequence generatePart(PartTOC p, int depth) {
        val article = EcoreUtil2.getContainerOfType(p, Article)
        var number = 0
        '''
        <h«depth»>Table of Contents</h«depth»>
        «FOR e: article.parts.filter[!(it instanceof PartTOC) && hasName(it)] SEPARATOR "<br/>"»
            «switch e {
                PartSection: generateTOCEntry(e.target, #[number += 1], false)
                PartTaxonomy: generateTOCEntry(e.target, #[number += 1], false)
                PartLexicon: generateTOCEntry(e.target, #[number += 1], false)
            }»
        «ENDFOR»
        «IF !article.appendix.nullOrEmpty»
        «if (number > 0) { number = 0; '</br>' } else ''»Appendix<br/>
        «FOR e: article.appendix.filter[!(it instanceof PartTOC) && hasName(it)] SEPARATOR "<br/>"»
            «switch e {
                PartSection: generateTOCEntry(e.target, #[number += 1], true)
                PartTaxonomy: generateTOCEntry(e.target, #[number += 1], true)
                PartLexicon: generateTOCEntry(e.target, #[number += 1], true)
            }»
        «ENDFOR»
        «ENDIF»
        '''
    }
    def tocNumber(Iterable<Integer> number, boolean appendix) {
        if (number.nullOrEmpty) return ""
        '''«if (appendix) 'A' + number.head else number.head »«FOR n: number.tail».«n»«ENDFOR»«IF number.size == 1».«ENDIF»'''
    }
    dispatch def CharSequence generateTOCEntry(
        Section s, Iterable<Integer> number, boolean appendix) {
        '''
        <a href="#section$«s.name»">«tocNumber(number, appendix)» «title(s)»</a>
        «FOR p: s.parts.filter[hasName(it)].indexed»<br/>
        «generateTOCEntry(p.value, number + #[p.key + 1], appendix)»«ENDFOR»
        '''   
    }
    dispatch def CharSequence generateTOCEntry(
        Taxon t, Iterable<Integer> number, boolean appendix) {
        '''
        <a href="#taxon$«t.name»">«tocNumber(number, appendix)» «title(t)»</a>
        '''   
    }
    dispatch def CharSequence generateTOCEntry(
        Lexicon l, Iterable<Integer> number, boolean appendix) {
        '''
        <a href="#lexicon$«l.name»">«tocNumber(number, appendix)» «title(l)»</a>
        '''   
    }
	
	dispatch def CharSequence generateDeclaration(Section s, int depth) {
		val classes = #["section"] + (richString2String(s.html.class_)?.toString?.split(" ") ?: newArrayOfSize(0))
		'''
		<section id="section$«s.name»" class="«classes.join(" ")»">
		<h«depth»>«title(s)»</h«depth»>
		«generatePostFuncs(generateRichString(s.text, true))»
		«FOR p: s.parts»
		«generateDeclaration(p, depth + 1)»
		«ENDFOR»
		</section>
		'''
	}
	dispatch def generateDeclaration(Lexicon l, int depth) {
		val classes = #["lexicon"] + (richString2String(l.html.class_)?.toString?.split(" ") ?: newArrayOfSize(0))
        '''<h«depth»«IF !l.name.nullOrEmpty» id="lexicon$«l.name»"«ENDIF» class="«classes.join(" ")»">«title(l)»</h1>
        «FOR m: l.morphemes»
        «generatePostFuncs(generateMorpheme(m, depth + 1))»
        «ENDFOR»'''
	}
	dispatch def generateDeclaration(Taxon t, int depth) {
        val classes = #["taxonomy"] + (richString2String(t.html.class_)?.toString?.split(" ") ?: newArrayOfSize(0))
        '''<h«depth»«IF !t.name.nullOrEmpty» id="taxonomy$«t.name»"«ENDIF» class="«classes.join(" ")»">«title(t)»</h1>
        «generateTaxon(t, depth + 1)»'''
	}
	dispatch def generateDeclaration(Syntax s, int depth) {
		""
	}
	dispatch def generateDeclaration(Language l, int depth) {
		""
	}
	
	def generateMorpheme(Morpheme m, int depth) {
		val classes = #["morpheme"] + (richString2String(m.html.class_)?.toString?.split(" ") ?: newArrayOfSize(0))
		generatePostFuncs('''<div class="«classes.join(" ")»">
		<b id="morpheme$«m.language.name»$«m.name»">«m.formStrings.join(" / ")»</b>
		«FOR t: m.taxons SEPARATOR " "»<a href="#taxon$«t.target.name»">«t.target.name»</a>«ENDFOR»
		«FOR e: m.entries.filter[type == "translation"] BEFORE "<p>Translations:<dl>\n" AFTER "</dl></p>"»
		<dd class="morpheme-translation">«generateRichString(e.language.title, false)»</dd>
		<dt class="morpheme-translation">«generateRichString(e.text, false)»<dt/>
		«ENDFOR»
		«FOR e: m.entries.filter[type == "derived"] BEFORE "<p>Derived from:<dl>\n" AFTER "</dl></p>"»
		<dd class="morpheme-derived">«generateRichString(e.language.title, false)»</dd>
		<dt class="morpheme-derived">«generateRichString(e.text, false)»<dt/>
		«ENDFOR»
		</div>''')
	}

	def CharSequence generateTaxon(Taxon t, int depth) {
        val classes = #["taxon"] + (richString2String(t.html.class_)?.toString?.split(" ") ?: newArrayOfSize(0))
        val parent = EcoreUtil2.getContainerOfType(t.eContainer, Taxon)
		generatePostFuncs('''<div class="«classes.join(" ")»">
		<b id="taxon$«t.name»">Taxon «t.name»</b>
		«IF parent !== null» <a href="#taxon$«parent.name»">«parent.name»</a>«ENDIF»
		«generateRichString(t.text, false)»
		«FOR tx: t.taxons»«generateTaxon(tx, depth + 1)»«ENDFOR»
		</div>''')
	}
}