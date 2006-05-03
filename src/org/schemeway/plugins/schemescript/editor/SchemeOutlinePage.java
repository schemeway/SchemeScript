/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.editor;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.BadPositionCategoryException;
import org.eclipse.jface.text.DefaultPositionUpdater;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IPositionUpdater;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;
import org.schemeway.plugins.schemescript.SchemeScriptPlugin;
import org.schemeway.plugins.schemescript.dictionary.SymbolEntry;

public class SchemeOutlinePage extends ContentOutlinePage implements ISchemeOutlinePage {
    
    private static final String MENU_DELIMITER = ";;;; --";
    private static final String SECTION_START = ";;;;   ";
    private static final String CHAPTER_START = ";;;; ";
    private static final String CHAPTER_CATEGORY = "___chapters";
    private static final String CHAPTER = "chapter";
    private static final String SECTION = "section";
    private static final String DEFINITION = "definition";
    private static Image CHAPTER_IMAGE;
    private static Image SECTION_IMAGE;
    private static Image DEFINITION_IMAGE;
//    private static ImageDescriptor DEFINITION_DESCRIPTOR;
//    private static ImageDescriptor CHAPTER_DESCRIPTOR;
    
    private static final SectionPositionComparator SECTION_COMPARATOR = new SectionPositionComparator();
//    private static final ShowDefinitionsOnlyFilter SHOW_DEFINITIONS_FILTER = new ShowDefinitionsOnlyFilter();

//    private ViewerFilter currentFilter = null; 
    
    private SchemeEditor mEditor;
//    private List mTreeElements;
    private Section mDefaultTree;
    
    private IPositionUpdater mPositionUpdater = new DefaultPositionUpdater(CHAPTER_CATEGORY);
    
    static {
        try {
            CHAPTER_IMAGE = new Image(Display.getDefault(), SchemeScriptPlugin.getDefault().find(new Path("icons/chapter.gif")).openStream());
            SECTION_IMAGE = new Image(Display.getDefault(), SchemeScriptPlugin.getDefault().find(new Path("icons/section.gif")).openStream());
            DEFINITION_IMAGE = new Image(Display.getDefault(), SchemeScriptPlugin.getDefault().find(new Path("icons/definition.gif")).openStream());
//            DEFINITION_DESCRIPTOR = ImageDescriptor.createFromImage(DEFINITION_IMAGE);
//            CHAPTER_DESCRIPTOR = ImageDescriptor.createFromImage(CHAPTER_IMAGE);
        }
        catch (Throwable exception) {
            CHAPTER_IMAGE = null;
            SECTION_IMAGE = null;
            DEFINITION_IMAGE = null;
        }
    }
    
    private static class Section {
        public String   name;
        public Position position;
        public List     children;
        public Section  parent;
        public String   type;
        
        public Section(String type, String name, Position position) {
            this.type = type;
            this.name = name;
            this.position = position;
            this.parent = null;
            this.children = new LinkedList();
        }
        
        public void addSubsection(Section section) {
            section.parent = this;
            for(int i=0; i<children.size(); i++) {
            	Section child = (Section) children.get(i);
            	if (child.position.offset > section.position.offset) {
            		children.add(i, section);
            		return;
            	}
            }
            children.add(section);
        }
        
        public Section[] getChildren() {
            return (Section[]) children.toArray(new Section[children.size()]);
        }
        
        public int size() {
        	return children.size();
        }
        
        public static Section createSection(String name, Position position) {
            return new Section(SECTION, name, position);
        }

        public static Section createChapter(String name, Position position) {
            return new Section(CHAPTER, name, position);
        }
        
        public static Section createDefinition(String name, Position position) {
            return new Section(DEFINITION, name, position);
        }
        
        public Object clone() {
            return new Section(type, name, position);
        }
    }
    
    private static class SectionPositionComparator implements Comparator {
        public int compare(Object o1, Object o2) {
            Section s1 = (Section)o1;
            Section s2 = (Section)o2;
            
            if (s1.position.offset < s2.position.offset)
                return -1;
            else if (s1.position.offset > s2.position.offset)
                return 1;
            else
                return 0;
        }
    }
    
//    private static class ShowDefinitionsOnlyFilter extends ViewerFilter {
//
//        public boolean select(Viewer viewer, Object parentElement, Object element)
//        {
//            Section section = (Section) element;
//            return DEFINITION == section.type;
//        }
//    }
    
//    private class ShowDefinitionsOnlyAction extends Action {
//        public ShowDefinitionsOnlyAction () {
//            super("Definitions", Action.AS_CHECK_BOX);
//            setToolTipText("Show definitions only");
//            setImageDescriptor(DEFINITION_DESCRIPTOR);
//            setChecked(false);
//        }
//        public void run() {
//            TreeViewer viewer = getTreeViewer();
//            if (currentFilter == null) {
//                currentFilter = SHOW_DEFINITIONS_FILTER;
//                viewer.addFilter(SHOW_DEFINITIONS_FILTER);
//                setChecked(true);
//            }
//            else {
//                currentFilter = null;
//                viewer.removeFilter(SHOW_DEFINITIONS_FILTER);
//                setChecked(false);
//            }
//        }
//    }
//    
//    private static class ShowHeadersOnlyFilter extends ViewerFilter {
//    	public boolean select(Viewer viewer, Object parentElement, Object element) {
//    		Section section = (Section) element;
//    		return DEFINITION != section.type;
//    	}
//    }
//    
//    private static class ShowHeadersOnlyAction extends Action {
//    	boolean checked = false;
//		public ShowHeadersOnlyAction() {
//            super("Headers", Action.AS_CHECK_BOX);
//            setToolTipText("Show headers only");
//            setImageDescriptor(CHAPTER_DESCRIPTOR);
//            setChecked(false);
//		}
//		public void run() {
//			if (checked) {
//				setChecked(false);
//				checked = false;
//			}
//			else {
//				setChecked(true);
//				checked = true;
//			}
//		}
//    }
    
    private static class SectionContentProvider implements ITreeContentProvider {
        
        public Object[] getChildren(Object parentElement)
        {
            if (parentElement != null) {
                return ((Section)parentElement).getChildren();
            }
            return null;
        }
        public Object getParent(Object element)
        {
            return ((Section)element).parent;
        }
        
        public boolean hasChildren(Object element)
        {
            return ((Section)element).children.size() > 0;
        }
        
        public Object[] getElements(Object inputElement)
        {
            return getChildren(inputElement);
        }
        
        public void dispose()
        {
        }
        
        public void inputChanged(Viewer viewer, Object oldInput, Object newInput)
        {
        }
    }
   
    private static class SectionLabelProvider extends LabelProvider {
        public String getText(Object element) {
            return ((Section)element).name;
        }
        
        public Image getImage(Object element)
        {
            Section section = (Section)element;
            if (section.type == SECTION) {
                return SECTION_IMAGE;
            }
            else if (section.type == CHAPTER ) {
                return CHAPTER_IMAGE;
            }
            
            return DEFINITION_IMAGE;
        }
    }
    
    public SchemeOutlinePage(SchemeEditor editor) {
        super();
        mEditor = editor;
    }
    
    public void createControl(Composite parent) {
        super.createControl(parent);
        
        TreeViewer viewer = getTreeViewer();
        viewer.setContentProvider(new SectionContentProvider());
        viewer.setLabelProvider(new SectionLabelProvider());
        Section tree = createTree();
        viewer.setInput(tree);
        Tree treeControl = (Tree) viewer.getControl();
        if (tree.size() > 0) {
        	TreeItem item = treeControl.getItem(0);
        	treeControl.setSelection(new TreeItem[] { item });
        }
        viewer.expandAll();
        
        
//        IToolBarManager manager = getSite().getActionBars().getToolBarManager();
//        manager.add(new ShowDefinitionsOnlyAction());
//        manager.add(new ShowHeadersOnlyAction());
    }
    
    private Section createTree() {
        Section root = null;
        try {
            IDocument document = mEditor.getDocument();

            cleanPositions(document);
            document.addPositionUpdater(mPositionUpdater);
            document.addPositionCategory(CHAPTER_CATEGORY);
            
            root = createRoot(document);

            List nodes = new LinkedList();
            addSections(document, nodes);
            addDefinitions(nodes);
            Collections.sort(nodes, SECTION_COMPARATOR);
            populateTree(root, nodes);
//            mTreeElements = nodes;
        }
        catch (BadLocationException exception) {
        }
        catch (BadPositionCategoryException exception) {
        }
        return root;
    }

	private Section createRoot(IDocument document) throws BadLocationException, BadPositionCategoryException {
		Section root;
		Position position;
		position = new Position(0,0);
		document.addPosition(CHAPTER_CATEGORY, position);
		root = Section.createChapter("Root", position);
		return root;
	}

    /**
     * @param root
     * @param nodes
     */
    private void populateTree(Section root, List nodes)
    {
        Section current = root;
        
        for (Iterator entries = nodes.iterator(); entries.hasNext();)
        {
            Section node = (Section) entries.next();
            if (node.type == CHAPTER) {
                root.addSubsection(node);
                current = node;
            }
            else if (node.type == SECTION) {
                if (current.type == SECTION) {
                    current = current.parent;
                }
                current.addSubsection(node);
                current = node;
            }
            else
                current.addSubsection(node);
        }
    }

    private void addSections(IDocument document, List nodes) throws BadLocationException, BadPositionCategoryException
    {
        Position position;
        int offset = 0;
        int documentLength = document.getLength();
        while (offset < documentLength) {
            ITypedRegion partition = document.getPartition(offset);
            if (partition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
                String text = document.get(partition.getOffset(), partition.getLength());
                text = text.trim();
                if (text.startsWith(SECTION_START)) {
                    position = new Position(offset + 7, text.length() - 7);
                    document.addPosition(CHAPTER_CATEGORY, position);
                    Section newSection = Section.createSection(text.substring(7), position);
                    nodes.add(newSection);
                }
                else if (text.startsWith(CHAPTER_START) && !(text.equals(MENU_DELIMITER))) {
                    position = new Position(offset + 5, text.length() - 5);
                    document.addPosition(CHAPTER_CATEGORY, position);
                    Section newSection = Section.createChapter(text.substring(5), position);
                    nodes.add(newSection);
                }
            }
            offset += partition.getLength();
        }
    }
    
    private void addDefinitions(List sections) throws BadLocationException {
        IDocument document = mEditor.getDocument();
        IEditorInput input = mEditor.getEditorInput();
        
        HashMap entriesAdded = new HashMap();

        if (input instanceof IFileEditorInput) {
            IFileEditorInput editorInput = (IFileEditorInput) input;
            List entries = mEditor.getSymbolDictionary().findSymbolForResource(editorInput.getFile());
            for (int index = 0; index < entries.size(); index++) {
                SymbolEntry entry = (SymbolEntry) entries.get(index);
                Section section = (Section) entriesAdded.get(entry);

                if (section != null)
                	continue;
                
                section = createSectionForEntry(document, entry);
                entriesAdded.put(entry , section);
                
            	SymbolEntry parent = entry.getParent();
            	while (parent != null) {
            		Section parentSection = (Section) entriesAdded.get(parent);
            		if (parentSection == null) {
            			parentSection = createSectionForEntry(document, parent);
                        entriesAdded.put(parent, parentSection);
            		}
            		parentSection.addSubsection(section);
            		section = parentSection;
            		parent = parent.getParent();
            	}
    
        		sections.add(section);
            }
        }
    }

	private Section createSectionForEntry(IDocument document, SymbolEntry entry) throws BadLocationException {
		int offset = document.getLineOffset(entry.getLineNumber() - 1);
		Position position = new Position(offset);
		document.addPosition(position);
		Section section = Section.createDefinition(entry.getName(), position);
		return section;
	}


    private void cleanPositions(IDocument document)
    {
        try {
            document.removePositionCategory(CHAPTER_CATEGORY);
            document.removePositionUpdater(mPositionUpdater);
        }
        catch (BadPositionCategoryException exception) {
        }
    }

    public void update() {
        TreeViewer viewer = getTreeViewer();
        viewer.getControl().setRedraw(false);
        mDefaultTree = createTree();
        getTreeViewer().setInput(mDefaultTree);
        viewer.expandAll();
        viewer.getControl().setRedraw(true);
    }
    
    public void selectionChanged(SelectionChangedEvent event) {
        super.selectionChanged(event);

        ISelection selection= event.getSelection();
        if (selection.isEmpty())
            mEditor.resetHighlightRange();
        else {
            Section section = (Section) ((IStructuredSelection) selection).getFirstElement();
            mEditor.setSelection(section.position.offset, section.position.offset + section.position.length);
        }
    }
}
