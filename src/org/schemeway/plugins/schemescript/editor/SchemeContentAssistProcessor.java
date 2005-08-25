/*
 * Copyright (c) 2002-2003 Nu Echo Inc. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor;

import java.util.*;

import org.eclipse.core.resources.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.contentassist.*;
import org.eclipse.jface.text.templates.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.ui.*;
import org.eclipse.ui.part.*;
import org.schemeway.plugins.schemescript.dictionary.*;

/**
 * @author Nu Echo Inc.
 */
public class SchemeContentAssistProcessor implements IContentAssistProcessor {
    
    private SchemeEditor mEditor;
    private IResource mResource;
    
    private static final char[] TRIGGER_CHARS = new char[] { '\n', ' ' };

    private static class SchemeCompletionProposal implements ICompletionProposal, ICompletionProposalExtension, ICompletionProposalExtension3 {
        CompletionProposal mDelegate;
        String mInsertion;
        String mSymbol;
        int mPriority;
        int mStartOffset;
        int mSymbolStartOffset;

        public SchemeCompletionProposal(String symbol, String insertion, int offset, int priority) {
            mDelegate = new CompletionProposal(insertion, offset, 0, insertion.length(), null, symbol, null, null);
            mStartOffset = offset;
            mInsertion = insertion;
            mPriority = priority;
            mSymbol = symbol;
            mSymbolStartOffset = offset - (symbol.length() - insertion.length());
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
            return mStartOffset;
        }

        public CharSequence getPrefixCompletionText(IDocument document, int completionOffset) {
            return mInsertion;
        }
        
        public int getPriority() {
            return mPriority;
        }

		public void apply(IDocument document, char trigger, int offset) {
			int delta = offset - mStartOffset;
			if (delta < 0)
				delta = 0;
			try {
				document.replace(mStartOffset, delta, mInsertion);
			} catch (BadLocationException x) {
				// ignore
			}
		}

		public boolean isValidFor(IDocument document, int offset) {
			try {
				int len = offset - mSymbolStartOffset;
				if (len <= 0)
					return false;
				String prefix = document.get(mSymbolStartOffset, len);
				return (mSymbol.startsWith(prefix));
			}
			catch (BadLocationException x) {
				// ignore
			}
			return false;
		}

		public char[] getTriggerCharacters() {
			return TRIGGER_CHARS;
		}

		public int getContextInformationPosition() {
			return mSymbolStartOffset;
		}
    }

    private static class ProposalComparator implements Comparator {
        public int compare(Object o1, Object o2) {
            if (o1 instanceof SchemeCompletionProposal && o2 instanceof SchemeCompletionProposal) {
                SchemeCompletionProposal p1 = (SchemeCompletionProposal) o1;
                SchemeCompletionProposal p2 = (SchemeCompletionProposal) o2;
                if (p1.getPriority() == p2.getPriority())
                    return p1.getDisplayString().compareTo(p2.getDisplayString());
                else if (p1.getPriority() < p2.getPriority())
                    return -1;
                else
                    return 1;
                }
            else {
                ICompletionProposal p1 = (ICompletionProposal) o1;
                ICompletionProposal p2 = (ICompletionProposal) o2;
                return p1.getDisplayString().compareTo(p2.getDisplayString());
                
            }
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

    public SchemeContentAssistProcessor(SchemeEditor editor) {
        mEditor = editor;
        setupResource();
    }
    
    protected SchemeEditor getEditor() {
        return mEditor;
    }
    
    protected IResource getResource() {
        return mResource;
    }
    
    protected void setupResource() {
        IEditorInput input = getEditor().getEditorInput();
        if (input instanceof FileEditorInput) {
            FileEditorInput fileInput = (FileEditorInput)input;
            mResource = fileInput.getFile();
        }
    }

    public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer, int offset) {
        ICompletionProposal[] result = null;
        try {
            String symbol = SchemeTextUtilities.findSymbolBeforePoint(viewer, offset);
            if (symbol != null) {
                ISymbolDictionary dictionary = getEditor().getSymbolDictionary();
                List proposals = new LinkedList();
                int len = symbol.length();
                SymbolEntry[] matchingEntries = dictionary.completeSymbol(symbol);
                for (int index = 0; index < matchingEntries.length; index++) {
                    SymbolEntry entry = matchingEntries[index];
                    String insertion = entry.getName().substring(len);
                    int priority = entry.getPriority();
                    if (entry.getFile() != null && entry.getFile() == getResource()) {
                        priority += 10;
                    }
                    proposals.add(new SchemeCompletionProposal(entry.getName(), insertion, offset, priority));
                }
                // add templates
                TemplateContextType type = dictionary.getTemplateContextType();
                TemplateContext context = new DocumentTemplateContext(type, getEditor().getDocument(), offset - len, len);
                Template[] templates = dictionary.completeTemplates(symbol);
                for (int index = 0; index < templates.length; index++)  {
                    proposals.add(new TemplateProposal(templates[index], context, new Region(offset, len), null));
                }
                
                if (proposals.size() != 0) {
                    Collections.sort(proposals, new ProposalComparator());
                    proposals = removeDuplicates(proposals);
                    result = (ICompletionProposal[]) proposals.toArray(new ICompletionProposal[proposals.size()]);
                }
            }
        }
        catch (BadLocationException exception) {
        }
        return result;
    }

    private List removeDuplicates(List list) {
        int listLength = list.size();
        List newList = new ArrayList(listLength);
        Set nameSet = new HashSet();
        
        for (int i=0; i<listLength; i++) {
            ICompletionProposal proposal = (ICompletionProposal)list.get(i);
            String name = proposal.getDisplayString();
            if (!nameSet.contains(name)) {
                nameSet.add(name);
                newList.add(proposal);
            }
        }
        
        return newList;
    }
    
    public IContextInformation[] computeContextInformation(ITextViewer viewer, int offset) {
        IContextInformation[] result = null;
        try {
            String symbol = SchemeTextUtilities.findSymbolAroundPoint(viewer.getDocument(), offset);
            if (symbol != null) {
                List informations = new LinkedList();
                SymbolEntry[] matchingEntries = getEditor().getSymbolDictionary().findSymbol(symbol);
                for (int index = 0; index < matchingEntries.length; index++) {
                    informations.add(makeContextInfo(matchingEntries[index]));
                }

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
        String context = info + "- [" + entry.getContext() + "]";
        return new ContextInformation(context, info);
    }
}
