package org.hanashiconlang

import org.eclipse.xtext.naming.IQualifiedNameProvider
import org.eclipse.xtext.naming.IQualifiedNameConverter
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.util.SimpleAttributeResolver
import com.google.inject.Inject
import org.hanashiconlang.hanashi.Document
import org.eclipse.xtext.EcoreUtil2
import org.hanashiconlang.hanashi.Rule

class HanashiQualifiedNameProvider extends IQualifiedNameProvider.AbstractImpl {
    
    @Inject
    val IQualifiedNameConverter qualifiedNameConverter = null;
    
    override QualifiedName getFullyQualifiedName(EObject obj) {
        if (obj instanceof Document)
            return qualifiedNameConverter.toQualifiedName(obj.eResource.URI.trimFileExtension.lastSegment)
        else {
            val name = SimpleAttributeResolver.NAME_RESOLVER.apply(obj);
            if(name === null || name.length() == 0)
                return null;
            val result = qualifiedNameConverter.toQualifiedName(name);
            if (obj.eContainer === null)
                return result
            else if (obj instanceof Rule)
                return getFullyQualifiedName(obj.eContainer).append(result)
            else
                return getFullyQualifiedName(EcoreUtil2.getContainerOfType(obj, Document)).append(result)
       }
    }
}