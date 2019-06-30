package org.hanashiconlang.generator

import java.util.ArrayList
import java.util.function.Function
import java.util.regex.Pattern
import org.eclipse.emf.common.util.EList
import org.eclipse.xtext.EcoreUtil2
import org.eclipse.xtext.util.Tuples
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
import org.hanashiconlang.hanashi.RichString
import org.hanashiconlang.hanashi.RichStringLiteral
import org.hanashiconlang.hanashi.Section
import org.hanashiconlang.hanashi.Syntax
import org.hanashiconlang.hanashi.Table
import org.hanashiconlang.hanashi.TableCol
import org.hanashiconlang.hanashi.TableRow
import org.hanashiconlang.hanashi.Taxon
import org.eclipse.xtext.util.Pair

import static extension org.hanashiconlang.HanashiExtensions.*
import java.util.HashMap
import org.hanashiconlang.hanashi.Translated

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
	dispatch def generateRichStringExpression(Gloss g, boolean trimLeft, boolean trimRight) { 
		val classes = #["gloss"] + (richString2String(g.html.class_)?.toString?.split(" ") ?: newArrayOfSize(0))
		Tuples.create('''
		<table class="«classes.join(" ")»">
			«FOR l: g.lines»
    		    <tr class="gloss-words">«generateGlossWordsText(l.words)»</tr>«
                IF (l.words.exists[items.exists[it instanceof GlossMorpheme]])
	                »<tr class="gloss-info">«
			    		generateGlossWordsInfo(l.words)
		    		»</tr>«
                ENDIF»
		    «ENDFOR»
		</table>''', false)
	}
	dispatch def generateRichStringExpression(Call c, boolean trimLeft, boolean trimRight) {
	    val func = richString2String(c.function).toString
	    val field = HanashiFunctions.getDeclaredField(func)
	    Tuples.create((field.get(null) as HanashiFunction).generate(
	        c.arguments.map[generateRichString(it, false)]
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
		if (!footnoteHash.containsKey(name)) {
			footnoteHash.put(name, Tuples.create(footnoteHumanCounter, footnoteGlobalCounter))
			footnoteHumanCounter += 1
			footnoteGlobalCounter += 1
		}
		val counters = footnoteHash.get(name)
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
            c.arguments.map[richStringExpression2String(it)]
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
	
	def generateGlossWordsText(EList<GlossWord> gws) 
		'''«FOR gw: gws»<td style="padding:0.1em" «
		  IF gw.skips.size > 0»colspan="«gw.skips.size + 1»"«ENDIF»>«
		  FOR gi: gw.items.indexed»«
		      IF gi.key > 0 && gi.value instanceof GlossMorpheme && 
		              gw.items.get(gi.key - 1) instanceof GlossMorpheme»«ENDIF»«
              generateGlossText(gi.value)»«
          ENDFOR
		  »</td>«ENDFOR»'''
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
	def generateGlossWordsInfo(EList<GlossWord> gws) 
		'''«FOR gw: gws»<td>«FOR gi: gw.items.indexed»«
                IF gi.key > 0 && gi.value instanceof GlossMorpheme && 
                        gw.items.get(gi.key - 1) instanceof GlossMorpheme».«ENDIF»«
                generateGlossInfo(gi.value)»«ENDFOR»</td>«
        ENDFOR»'''
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
	
	dispatch def CharSequence generateDeclaration(Section s, int depth) {
		val classes = #["section"] + (richString2String(s.html.class_)?.toString?.split(" ") ?: newArrayOfSize(0))
		'''<h«depth» id="section$«s.name»" class="«classes.join(" ")»">«generateRichString(s.title, false)»</h1>
		«generatePostFuncs(generateRichString(s.text, true))»
		«FOR p: s.parts»
		«generateDeclaration(p, depth + 1)»
		«ENDFOR»'''
	}
	dispatch def generateDeclaration(Lexicon l, int depth) {
		val classes = #["lexicon"] + (richString2String(l.html.class_)?.toString?.split(" ") ?: newArrayOfSize(0))
        '''<h«depth»«IF !l.name.nullOrEmpty» id="lexicon$«l.name»"«ENDIF» class="«classes.join(" ")»">Lexicon for «generateRichString(l.language.title, false)»</h1>
        «FOR m: l.morphemes»
        «generatePostFuncs(generateMorpheme(m, depth + 1))»
        «ENDFOR»'''
	}
	dispatch def generateDeclaration(Taxon t, int depth) {
        val classes = #["taxonomy"] + (richString2String(t.html.class_)?.toString?.split(" ") ?: newArrayOfSize(0))
        '''<h«depth»«IF !t.name.nullOrEmpty» id="taxonomy$«t.name»"«ENDIF» class="«classes.join(" ")»">Taxonomy</h1>
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