package org.hanashiconlang.conversion

import org.eclipse.xtext.AbstractRule
import org.eclipse.xtext.common.services.DefaultTerminalConverters
import org.eclipse.xtext.conversion.ValueConverter
import org.eclipse.xtext.conversion.impl.STRINGValueConverter
import org.eclipse.xtext.nodemodel.INode

class HanashiValueConverterService extends DefaultTerminalConverters {
    
    val richTextConverter = new STRINGValueConverter {
        var AbstractRule rule
        override toValue(String string, INode node) {
            if (string !== null && string.length >= 2)
                super.toValue("\"" + string.substring(1, string.length - 1) + "\"", node)
            else
                super.toValue(string, node)
        }
        override toString(String value) {
            val result = super.toString(value)
            switch rule.name {
            case "TEXTLEFT": result.substring(0, result.length - 1) + "$"
            case "TEXTMIDDLE": "$" + result.substring(1, result.length - 1) + "$"
            case "TEXTRIGHT": "$" + result.substring(1, result.length)
            default: result
            }                
        }
        override setRule(AbstractRule rule) {
            this.rule = rule
            super.setRule(rule)
        }
    }
    @ValueConverter(rule = "TEXTSINGLE")
    def getRichTextSingleConverter() {
        richTextConverter        
    }
    @ValueConverter(rule = "TEXTLEFT")
    def TEXTLEFT() {
        richTextConverter        
    }
    @ValueConverter(rule = "TEXTMIDDLE")
    def getRichTextMiddleConverter() {
        richTextConverter        
    }
    @ValueConverter(rule = "TEXTRIGHT")
    def getRichTextRightConverter() {
        richTextConverter        
    }
    
}