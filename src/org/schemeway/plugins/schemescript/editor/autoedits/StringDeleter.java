/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor.autoedits;

import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.editor.*;

public class StringDeleter implements IAutoEditStrategy {

    public StringDeleter() {
        super();
    }

    public void customizeDocumentCommand(IDocument document, DocumentCommand command) {
        try {
            if (command.length == 1 && command.text.length() == 0) {
                ITypedRegion partition = document.getPartition(command.offset);

                if (partition.getType() == SchemePartitionScanner.SCHEME_STRING) {
                    if (command.offset == partition.getOffset()
                            || command.offset == (partition.getOffset() + partition.getLength() - 1)) {
                        command.offset = partition.getOffset();
                        command.length = partition.getLength();
                    }
                    else {
                        char currentChar = document.getChar(command.offset);
                        if (currentChar == '\\' && command.offset < partition.getOffset() + partition.getLength() - 1) {
                            // remove '\' and the next character
                            command.length = 2;
                        }
                        else if (currentChar == '"') {
                            command.offset -= 1;
                            command.length = 2;
                        }
                    }
                }
            }
            else if (command.length > 1 && command.text.length() == 0) {
                int start = command.offset;
                int end = start + command.length;
                
                ITypedRegion startPartition = document.getPartition(start);
                ITypedRegion endPartition   = document.getPartition(end);
                
                if (startPartition.getType() == SchemePartitionScanner.SCHEME_STRING)
                    start = startPartition.getOffset();
                if (endPartition.getType() == SchemePartitionScanner.SCHEME_STRING) 
                    end = endPartition.getOffset() + endPartition.getLength();
                
                command.offset = start;
                command.length = end - start;
            }
        }
        catch (BadLocationException exception) {
        }
    }
}