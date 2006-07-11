/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor.autoedits;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;

public class SexpDeleter implements IAutoEditStrategy {

    /*
     * TODO
     *  - plutot que deleter les S-expressions englobantes, 
     *    deleter seulement lorsque les parenthèses "matchent"
     *  - Idem pour l'insertion...
     *  - Ajouter une action pour étendre la selection 
     *    aux S-expressions englobantes (l'équivalent actuel
     *    de SexpUtils.deleteSelection()
     */
    
    public SexpDeleter() {
        super();
    }

    public void customizeDocumentCommand(IDocument document, DocumentCommand command) {
        try {
            if (command.text.length() == 0 && command.length == 1) {
                char currentChar;
                currentChar = document.getChar(command.offset);
                if (currentChar == '(' || currentChar == '[') {
                    SexpUtils.deleteForwardSexp(document, command);
                }
                else if (currentChar == ')' || currentChar == ']') {
                    SexpUtils.deleteBackwardSexp(document, command);
                }
            }
            else if (command.text.length() == 0 && command.length > 1) {
                ITypedRegion startPartition = SchemeTextUtilities.getPartition(document, command.offset);
                // FIXME - This is a hack to prevent already deleted characters from CommentDeleter
                // to be reinserted...
                if (startPartition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
                    if ((command.offset + command.length) <= (startPartition.getOffset() + startPartition.getLength()))
                        return;
                }
                if (SexpUtils.whitespacesOnly(document, command.offset, command.length))
                    return;
                SexpUtils.deleteSelection(document, command);
            }
            else if (command.text.length() > 1 && command.length == 0) {
                SexpUtils.insertText(document, command);
            }
        }
        catch (BadLocationException e) {
        }
    }
}