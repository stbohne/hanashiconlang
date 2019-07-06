package org.hanashiconlang.generator

import org.eclipse.xtext.util.Tuples
import org.eclipse.xtext.util.Pair
import org.eclipse.xtext.validation.ValidationMessageAcceptor
import org.hanashiconlang.hanashi.RichString

interface HanashiFunction {
    public static val TAKES_NO_ARGUMENTS = "takesNoArguments"
    public static val REQUIRES_ONE_ARGUMENT = "requiresOneArgument"
    public static val REQUIRES_EVEN_ARGUMENTS = "requiresEvenArguments"

    def void validate(
        Iterable<RichString> text,
        HanashiRenderer renderer, 
        IHanashiFunctionValidationMessageAcceptor acceptor
    ) {}
    def CharSequence generate(Iterable<RichString> text,
        HanashiRenderer renderer)
    def CharSequence string(Iterable<RichString> text,
        HanashiRenderer renderer)
}
interface IHanashiFunctionValidationMessageAcceptor {
    def void error(String message, int index, String code, String... issueData)
    def void error(String message, String code, String... issueData) {
        error(message, ValidationMessageAcceptor.INSIGNIFICANT_INDEX, code, issueData)
    }
    def void warning(String message, int index, String code, String... issueData)
    def void warning(String message, String code, String... issueData) {
        warning(message, ValidationMessageAcceptor.INSIGNIFICANT_INDEX, code, issueData)
    }
    def void info(String message, int index, String code, String... issueData)
    def void info(String message, String code, String... issueData) {
        info(message, ValidationMessageAcceptor.INSIGNIFICANT_INDEX, code, issueData)
    }
}

abstract class NoArgumentFunction implements HanashiFunction {
    val String name
    new(String name) {
        this.name = name
    }
    override validate(Iterable<RichString> text,
        HanashiRenderer renderer,
        IHanashiFunctionValidationMessageAcceptor acceptor) {
        if (text.size != 0)
            acceptor.error("'" + name + "' takes no arguments",
                TAKES_NO_ARGUMENTS)
    }
    override generate(Iterable<RichString> text,
        HanashiRenderer renderer) {
        doGenerate()
    }
    override string(Iterable<RichString> text,
        HanashiRenderer renderer) {
        doString()
    }
    abstract def CharSequence doGenerate()
    abstract def CharSequence doString()
}

abstract class OneArgumentFunction implements HanashiFunction {
    val String name
    new(String name) {
        this.name = name
    }
    override validate(Iterable<RichString> text,
        HanashiRenderer renderer, 
        IHanashiFunctionValidationMessageAcceptor acceptor) {
        if (text.size != 1)
            acceptor.error("'" + name + "' requires exactly one argument",
                REQUIRES_ONE_ARGUMENT) 
    }
    override generate(Iterable<RichString> text,
        HanashiRenderer renderer) {
        doGenerate(text.head, renderer)
    }
    override string(Iterable<RichString> text,
        HanashiRenderer renderer) {
        doString(text.head, renderer)
    }
    abstract def CharSequence doGenerate(RichString text,
        HanashiRenderer renderer)
    abstract def CharSequence doString(RichString text,
        HanashiRenderer renderer)
}

class SurroundFunction extends OneArgumentFunction {
    val String before
    val String after
    new(String name, String before, String after) {
        super(name)
        this.before = before
        this.after = after
    }
    override doGenerate(RichString text,
        HanashiRenderer renderer) 
        '''«before»«renderer.generateRichString(text, false)»«after»'''
    override doString(RichString text,
        HanashiRenderer renderer) {
        '''«before»«renderer.richString2String(text)»«after»'''
    }
}

class SurroundMarkupFunction extends SurroundFunction {
    new(String name, String before, String after) {
        super(name, before, after)
    }
    override doString(RichString text,
        HanashiRenderer renderer) {
        '''«renderer.richString2String(text)»'''
    }
}

abstract class PairArgumentsFunction implements HanashiFunction {
    val String name
    new(String name) {
        this.name = name
    }
    override validate(Iterable<RichString> text,
        HanashiRenderer renderer, 
        IHanashiFunctionValidationMessageAcceptor acceptor) {
        if (text.size % 2 != 0)
            acceptor.error(
                "'" + name + "' requires an even number of arguments",
                REQUIRES_EVEN_ARGUMENTS
            )  
    }
    override generate(Iterable<RichString> text,
        HanashiRenderer renderer) {
        val pairs = <Pair<RichString, RichString>>newArrayOfSize(text.size / 2)
        val it = text.iterator
        var i = 0
        while (it.hasNext) {
            pairs.set(i, Tuples.create(it.next, it.next))
            i += 1
        }
        doGenerate(pairs.toList, renderer)
    }
    override string(Iterable<RichString> text,
        HanashiRenderer renderer) {
        val pairs = <Pair<RichString, RichString>>newArrayOfSize(text.size / 2)
        val it = text.iterator
        var i = 0
        while (it.hasNext) {
            pairs.set(i, Tuples.create(it.next, it.next))
            i += 1
        }
        doString(pairs, renderer)
    }
    abstract def CharSequence doGenerate(Iterable<Pair<RichString, RichString>> text,
        HanashiRenderer renderer)
    abstract def CharSequence doString(Iterable<Pair<RichString, RichString>> text,
        HanashiRenderer renderer)
}

class HanashiFunctions {
    public static val p = new OneArgumentFunction("p") {
        override doGenerate(RichString text,
            HanashiRenderer renderer) 
            '''«renderer.generateRichString(text, true)»'''
        override doString(RichString text,
            HanashiRenderer renderer) {
            '''«renderer.richString2String(text)»'''
        }    
    }
    public static val h = new PairArgumentsFunction("h") {
        override doGenerate(Iterable<Pair<RichString, RichString>> text,
                HanashiRenderer renderer) 
            '''«FOR tb: text»<b>«renderer.generateRichString(tb.first, false)»</b>
            «renderer.generateRichString(tb.second, true)»
            «ENDFOR»'''
        override doString(Iterable<Pair<RichString, RichString>> text,
                HanashiRenderer renderer) {
            '''«FOR tb: text SEPARATOR " "»«
                renderer.richString2String(tb.first)» «
                renderer.richString2String(tb.second)»«ENDFOR»'''
        }
    }
    public static val br = new NoArgumentFunction("br") { 
        override doGenerate() '''<br/>'''
        override doString() ''''''
    }
    public static val bo = new NoArgumentFunction("bo") {
        override doGenerate() { "{" }
        override doString() { "{" }
    }    
    public static val bc = new NoArgumentFunction("bc") {
        override doGenerate() { "}" }
        override doString() { "}" }
    }    
    public static val nl = new NoArgumentFunction("nl") {
        override doGenerate() { "\n" }
        override doString() { "\n" }
    }    
    public static val tab = new NoArgumentFunction("tab") {
        override doGenerate() { "\t" }
        override doString() { "\t" }
    }   
    public static val ul = new HanashiFunction {
        override generate(
            Iterable<RichString> text, 
            HanashiRenderer renderer
        ) '''
            <ul>«FOR i: text»<li>«renderer.generateRichString(i, false)»</li>«ENDFOR»</ul>
        '''
        override string(
            Iterable<RichString> text, 
            HanashiRenderer renderer
        ) '''
            «FOR i: text SEPARATOR "\n"»«renderer.richString2String(i)»«ENDFOR»
        '''
    } 
    public static val ol = new HanashiFunction {
        override generate(
            Iterable<RichString> text, 
            HanashiRenderer renderer
        ) '''
            <ol>«FOR i: text»<li>«renderer.generateRichString(i, false)»</li>«ENDFOR»</ol>
        '''
        override string(
            Iterable<RichString> text, 
            HanashiRenderer renderer
        ) '''
            «FOR i: text SEPARATOR "\n"»«renderer.richString2String(i)»«ENDFOR»
        '''
    } 
    public static val em = new SurroundMarkupFunction("em", "<em>", "</em>")
    public static val u = new SurroundMarkupFunction("u", "<u>", "</u>")
    public static val b = new SurroundMarkupFunction("b", "<b>", "</b>")
    public static val i = new SurroundMarkupFunction("i", "<i>", "</i>")
    public static val pt = new SurroundFunction("pt", "[", "]")
    public static val pm = new SurroundFunction("pm", "/", "/")
    public static val gm = new SurroundFunction("gm", "⟨", "⟩")
}

