/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.indentation;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.preference.*;
import org.eclipse.jface.text.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.dictionary.*;
import org.schemeway.plugins.schemescript.parser.*;
import org.schemeway.plugins.schemescript.preferences.*;

public class SchemeIndentationStrategy implements IAutoEditStrategy {
    private SexpNavigator            mExplorer;
    private SchemeIndentationManager mIndentationManager;

    public SchemeIndentationStrategy(SchemeIndentationManager indentManager) {
        Assert.isNotNull(indentManager);
        mIndentationManager = indentManager;
    }

    public void dispose() {
        mExplorer.dispose();
        mExplorer = null;
    }

    public void customizeDocumentCommand(IDocument document, DocumentCommand command) {
        initializeStrategy(document);
        if (command.length == 0 && command.text != null
                && TextUtilities.endsWith(document.getLegalLineDelimiters(), command.text) != -1) {
            try {
                IRegion lineInfo = document.getLineInformationOfOffset(command.offset);
                String line = document.get(lineInfo.getOffset(), lineInfo.getLength());
                String prefix = CommentPreferences.getCommentPrefix();
                IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
                boolean continueComment = store.getBoolean(CommentPreferences.COMMENT_CONTINUE);

                // make sure we are not at the start of the line, in which case
                // we simply insert a line break
                if (continueComment && line.startsWith(prefix) && (!line.equals(prefix + " "))
                        && command.offset != lineInfo.getOffset()) {
                    command.text = command.text + prefix + " ";
                } else {
                    autoIndentAfterNewLine(document, command);
                }
            } catch (BadLocationException exception) {

            }
        }
    }

    private void initializeStrategy(IDocument document) {
        if (mExplorer == null || document != mExplorer.getDocument()) {
            if (mExplorer != null)
                mExplorer.dispose();
            mExplorer = new SexpNavigator(document);
        }
    }

    private void autoIndentAfterNewLine(IDocument document, DocumentCommand command) {
        try {
            int indentation = findIndentation(new SchemeIndentationContext(mExplorer, mIndentationManager,
                    command.offset));
            if (indentation > 0) {
                StringBuffer buffer = new StringBuffer(command.text);

                for (int i = 0; i < indentation; i++)
                    buffer.append(' ');
                command.text = buffer.toString();
            }
            removeLeadingSpaces(document, command);
        } catch (BadLocationException exception) {
        }
    }

    public static int findIndentation(SchemeIndentationContext context) throws BadLocationException {
        int indentation = 0;
        SexpNavigator explorer = context.getExplorer();
        IDocument document = explorer.getDocument();

        if (explorer.backwardSexpression(context.getOffset())) {
            // There is an S-expression before the insertion point
            int previousStart = context.getExplorer().getSexpStart();

            if (explorer.upSexpression(previousStart)) {
                // We are inside an S-expression
                int outerStart = explorer.getSexpStart();
                char ch = document.getChar(outerStart);
                char firstCh = ch;
                while (!SchemeScannerUtilities.isOpeningParenthesis(ch)) {
                    outerStart++;
                    ch = document.getChar(outerStart);
                }
                indentation = findColumn(document, outerStart) + 1;
                if (SchemeScannerUtilities.isOpeningBracket(ch)) {
                    return indentation;
                }
                boolean constantList = isConstantListPrefix(firstCh);

                explorer.downSexpression(outerStart);
                explorer.forwardSexpression(explorer.getSexpStart());
                int firstSexpType = explorer.getSexpType();
                // we have a form '(symbol ...)'
                if (firstSexpType == SexpNavigator.TYPE_SYMBOL) {
                    // Find the indentation scheme
                    String text = explorer.getText();
                    IndentationRule scheme = context.getManager().getFunction(text);

                    indentation = findIndentationFromScheme(context.getExplorer(),
                                                            context.getOffset(),
                                                            previousStart,
                                                            outerStart,
                                                            scheme,
                                                            text,
                                                            constantList);
                } else if (firstSexpType == SexpNavigator.TYPE_LIST) {
                    indentation = findColumn(document, explorer.getSexpStart());
                } else if (firstSexpType == SexpNavigator.TYPE_CONSTANT || firstSexpType == SexpNavigator.TYPE_STRING) {
                    indentation = findColumn(document, explorer.getSexpStart());
                } else
                    indentation = findColumn(document, previousStart);
            } else {
                indentation = findColumn(document, previousStart);
            }   
        } else if (explorer.upSexpression(context.getOffset())) {
            indentation = findColumn(document, explorer.getSexpStart()) + 1;
        }
        return indentation;
    }

    /*
     * Returns true for characters starting a constant list (currently vector,
     * quoted and back-quoted list)
     */
    private static boolean isConstantListPrefix(char firstCh) {
        return firstCh == '\'' || firstCh == '`' || firstCh == '#';
    }

    private static int findIndentationFromScheme(SexpNavigator explorer,
                                                 int insertionOffset,
                                                 int previousStart,
                                                 int outerStart,
                                                 IndentationRule scheme,
                                                 String symbolText,
                                                 boolean constantList) throws BadLocationException {
        int indentation;
        String type = scheme.getCategory();
        IDocument document = explorer.getDocument();

        if (type == IndentationRule.DEFAULT) {
            // find the first no
            int lineStart = explorer.getDocument().getLineInformationOfOffset(previousStart).getOffset();
            int offset = previousStart;

            while (explorer.backwardSexpression(offset) && offset > lineStart) {
                previousStart = offset;
                offset = explorer.getSexpStart();
            }
            if (DictionaryUtils.findUserDefinitions(symbolText).length == 0) {
                indentation = findColumn(document, outerStart) + 1;
            } else {
                indentation = findColumn(document, previousStart);
            }
        } else if (type == IndentationRule.NONE) {
            indentation = findColumn(document, outerStart);
        } else if (type == IndentationRule.SEQUENCE || type == IndentationRule.DEFINITION) {
            indentation = findColumn(document, outerStart) + 2;
        } else if (type == IndentationRule.IF) {
            indentation = findColumn(document, outerStart) + 4;
        } else if (type == IndentationRule.WITH) {
            int previousCount = 0;
            int offset = insertionOffset;

            while (explorer.backwardSexpression(offset)) {
                offset = explorer.getSexpStart();
                previousCount++;
                if (previousCount > scheme.getHint())
                    break;
            }
            if (previousCount > scheme.getHint())
                indentation = findColumn(document, outerStart) + 2;
            else
                indentation = findColumn(document, outerStart) + 4;
        } else
            indentation = findColumn(document, previousStart);
        return indentation;
    }

    private static int findColumn(IDocument document, int offset) throws BadLocationException {
        IRegion info = document.getLineInformationOfOffset(offset);
        int tabWidth = SchemePreferences.getTabWidth();

        int indent = 0;
        int index = info.getOffset();
        while (index < offset) {
            char ch = document.getChar(index++);
            if (ch == '\t')
                indent += tabWidth;
            else
                indent++;
        }
        return indent;
    }

    private void removeLeadingSpaces(IDocument document, DocumentCommand command) throws BadLocationException {
        command.length = indentationLength(document, command.offset);
    }

    public static int indentationLength(IDocument document, int offset) throws BadLocationException {
        int end = document.getLength();
        int length = 0;
        while (offset < end) {
            char ch = document.getChar(offset++);
            if (ch == ' ' || ch == '\t') {
                length++;
            } else {
                break;
            }
        }
        return length;
    }

    public static String makeIndentationString(int length) {
        StringBuffer buffer = new StringBuffer(0);
        if (length > 0) {
            while (length-- > 0)
                buffer.append(' ');
        }
        return buffer.toString();
    }
}