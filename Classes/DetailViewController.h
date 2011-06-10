//
//  DetailViewController.h
//  Lisp
//
//  Created by David Pollak on 6/7/11.
//  Copyright 2011 lift Web Framework. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface DetailViewController : UIViewController <UIPopoverControllerDelegate, UISplitViewControllerDelegate> {
    
    UIPopoverController *popoverController;
    UIToolbar *toolbar;
    
    id detailItem;
    UILabel *detailDescriptionLabel;
	
	@public void (*cb_lispEval)(void* vc, const char* touch);
}

@property (nonatomic, retain) IBOutlet UIToolbar *toolbar;

@property (nonatomic, retain) id detailItem;
@property (nonatomic, retain) IBOutlet UILabel *detailDescriptionLabel;

@property (nonatomic, retain) IBOutlet UITextField *textField;
@property (nonatomic, retain) IBOutlet UITextView *tableDude;

-(IBAction)buttonPressed: (id)sender;

@end
