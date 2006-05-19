/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.views;

import org.eclipse.jface.action.*;
import org.eclipse.jface.resource.*;
import org.eclipse.swt.*;
import org.eclipse.swt.custom.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.eclipse.ui.part.*;
import org.schemeway.plugins.schemescript.*;

public class KawaScratchpadView extends ViewPart {
	private static final String VIEW_ID = SchemeScriptPlugin.PLUGIN_NS + ".kawa.scratchpadView";
	private CTabFolder mFolder;

	private static ImageDescriptor mRemoveImageDescriptor = SchemeScriptImageRegistry
			.getImageDescriptor("icons/remove.gif");
	private static ImageDescriptor mRemoveAllImageDescriptor = SchemeScriptImageRegistry
			.getImageDescriptor("icons/removeAll.gif");

	private class RemoveAllAction extends Action {
		public RemoveAllAction() {
			super(null, Action.AS_PUSH_BUTTON);
			setToolTipText("Remove all");
			setImageDescriptor(mRemoveAllImageDescriptor);
		}

		public void run() {
			CTabItem[] items = mFolder.getItems();
			for (int index = 0; index < items.length; index++) {
				items[index].dispose();
			}
		}
	}

	private class RemoveAction extends Action {
		public RemoveAction() {
			super(null, Action.AS_PUSH_BUTTON);
			setToolTipText("Remove");
			setImageDescriptor(mRemoveImageDescriptor);
		}

		public void run() {
			int index = mFolder.getSelectionIndex();
			if (index >= 0) {
				CTabItem item = mFolder.getItem(index);
				item.dispose();
			}
		}
	}

	public KawaScratchpadView() {
		super();
	}

	public void createPartControl(Composite parent) {
		mFolder = new CTabFolder(parent, SWT.BOTTOM);

		IToolBarManager manager = this.getViewSite().getActionBars().getToolBarManager();
		manager.add(new RemoveAction());
		manager.add(new RemoveAllAction());
	}

	public void setFocus() {
		mFolder.setFocus();
	}

	private void addNewView(String name, Control control) {
		CTabItem item = new CTabItem(mFolder, SWT.BORDER);
		item.setControl(control);
		item.setText(name);
	}

	public static Composite getControl() {
		try {
			KawaScratchpadView view = (KawaScratchpadView) (PlatformUI.getWorkbench()
					.getActiveWorkbenchWindow().getActivePage().showView(VIEW_ID));
			return view.mFolder;
		}
		catch (PartInitException e) {
		}
		return null;
	}

	public static void addView(String name, Control control) {
		try {
			KawaScratchpadView view = (KawaScratchpadView) (PlatformUI.getWorkbench()
					.getActiveWorkbenchWindow().getActivePage().showView(VIEW_ID));
			view.addNewView(name, control);
		}
		catch (PartInitException e) {
			SchemeScriptPlugin.logException("Unable to add control", e);
		}
	}
}