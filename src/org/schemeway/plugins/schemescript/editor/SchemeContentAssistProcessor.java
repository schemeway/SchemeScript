/*
 * Copyright (c) 2002-2003 Nu Echo Inc. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor;

import java.util.*;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.contentassist.*;
import org.eclipse.swt.graphics.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.dictionary.*;
import org.schemeway.plugins.schemescript.parser.*;

/**
 * @author Nu Echo Inc.
 */
public class SchemeContentAssistProcessor implements IContentAssistProcessor {

    private static class SchemeCompletionProposal implements ICompletionProposal, ICompletionProposalExtension3 {
        CompletionProposal mDelegate;
        String mInsertion;
        int mPriority;

        public SchemeCompletionProposal(String symbol, String insertion, int offset, int priority) {
            mDelegate = new CompletionProposal(insertion, offset, 0, insertion.length(), null, symbol, null, null);
            mInsertion = insertion;
            mPriority = priority;
        }

        public void apply(IDocument document) {
            mDelegate.apply(document);
        }

        public String getAdditionalProposalInfo() {
            return mDelegate.getAdditionalProposalInfo();
        }

        public IContextInformation getContextInformation() {
            return mDelegate.getContextInformation();
        }

        public String getDisplayString() {
            return mDelegate.getDisplayString();
        }

        public Image getImage() {
            return mDelegate.getImage();
        }

        public Point getSelection(IDocument document) {
            return mDelegate.getSelection(document);
        }

        public IInformationControlCreator getInformationControlCreator() {
            return null;
        }

        public int getPrefixCompletionStart(IDocument document, int completionOffset) {
            return completionOffset;
        }

        public CharSequence getPrefixCompletionText(IDocument document, int completionOffset) {
            return mInsertion;
        }
        
        public int getPriority() {
            return mPriority;
        }
    }

    private static class ProposalComparator implements Comparator {
        public int compare(Object o1, Object o2) {
            SchemeCompletionProposal p1 = (SchemeCompletionProposal) o1;
            SchemeCompletionProposal p2 = (SchemeCompletionProposal) o2;
            if (p1.getPriority() == p2.getPriority())
                return p1.getDisplayString().compareTo(p2.getDisplayString());
            else if (p1.getPriority() < p2.getPriority())
                return -1;
            else
                return 1;
        }
    }

    private static class Validator implements IContextInformationValidator {
        private int mInstallOffset;
        public boolean isContextInformationValid(int offset) {
            return Math.abs(mInstallOffset - offset) < 5;
        }
        public void install(IContextInformation info, ITextViewer viewer, int offset) {
            mInstallOffset = offset;
        }
    }

    public SchemeContentAssistProcessor() {
    }

    public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer, int offset) {
        ICompletionProposal[] result = null;
        try {
            String symbol = SchemeTextUtilities.findSymbolBeforePoint(viewer, offset);
            if (symbol != null) {
                List proposals = new LinkedList();
                int len = symbol.length();
                SymbolEntry[] matchingEntries = SchemeScriptPlugin.getDefault().getDictionary().completeSymbol(symbol);
                for (int index = 0; index < matchingEntries.length; index++) {
                    SymbolEntry entry = matchingEntries[index];
                    String insertion = entry.getName().substring(len);
                    proposals.add(new SchemeCompletionProposal(entry.getName(), insertion, offset, entry.getPriority()));
                }
                if (proposals.size() != 0) {
                    Collections.sort(proposals, new ProposalComparator());
                    result = (ICompletionProposal[]) proposals.toArray(new ICompletionProposal[proposals.size()]);
                }
            }
        }
        catch (BadLocationException exception) {
        }
        return result;
    }

    public IContextInformation[] computeContextInformation(ITextViewer viewer, int offset) {
        IContextInformation[] result = null;
        try {
            String symbol = SchemeTextUtilities.findSymbolAroundPoint(viewer, offset);
            if (symbol != null) {
                List informations = new LinkedList();
                SymbolEntry[] matchingEntries = SchemeScriptPlugin.getDefault().getDictionary().findSymbol(symbol);
                for (int index = 0; index < matchingEntries.length; index++)
                    informations.add(makeContextInfo(matchingEntries[index]));

                if (informations.size() != 0) {
                    result = (IContextInformation[]) informations.toArray(new IContextInformation[informations.size()]);
                }
            }
        }
        catch (BadLocationException exception) {
        }
        return result;
    }

    public char[] getCompletionProposalAutoActivationCharacters() {
        return new char[] {
            '-', '.', ':'
        };
    }

    public char[] getContextInformationAutoActivationCharacters() {
        return new char[]  { ' ' };
    }

    public String getErrorMessage() {
        return null;
    }

    public IContextInformationValidator getContextInformationValidator() {
        return new Validator();
    }

    
    private IContextInformation makeContextInfo(SymbolEntry entry) {
        String info = entry.getDescription();
        String context = "[" + entry.getCategory() + "] " + info;
        return new ContextInformation(context, info);
    }
}
