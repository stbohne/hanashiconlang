package org.hanashiconlang.ui.syntaxcoloring

import org.eclipse.xtext.ui.editor.syntaxcoloring.DefaultAntlrTokenToAttributeIdMapper
import org.eclipse.xtext.ide.editor.syntaxcoloring.HighlightingStyles

class HanashiTokenToAttributeMapper extends DefaultAntlrTokenToAttributeIdMapper {
	
	override protected String calculateId(String tokenName, int tokenType) {
		if (tokenName.contains("TEXT"))
			HighlightingStyles.STRING_ID
		else
			super.calculateId(tokenName, tokenType)
	}
	
}