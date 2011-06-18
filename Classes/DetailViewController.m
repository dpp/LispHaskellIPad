//
//  DetailViewController.m
//  Lisp
//
//  Created by David Pollak on 6/7/11.
//  Copyright 2011 lift Web Framework. All rights reserved.
//

#import "DetailViewController.h"
#import "RootViewController.h"
#import "PerformOMatic.h"

static void (*cb_viewDidLoad)(void*);

void setViewDidLoad(void (*fp)(void*))
{
    cb_viewDidLoad = fp;
}

void addToResult(DetailViewController* vc, char *str)
{
	NSString *resStr = [[NSString alloc] initWithCString:str];

	NSString *curText = vc.tableDude.text;
	vc.tableDude.text = [[curText stringByAppendingString:@": "] stringByAppendingString:resStr];
}

void setLispEval(DetailViewController *vc, void (*cb)(void *vc, const char *))
{
	vc -> cb_lispEval = cb;
}

void dispatchFunc(void (*fp)(void*)) {
	// dispatch_async_f(dispatch_get_main_queue(), NULL, fp);
	id runner = [[PerformOMatic alloc] init];
	[runner setFunc:fp];
	[runner run];
}

@interface DetailViewController ()
@property (nonatomic, retain) UIPopoverController *popoverController;
- (void)configureView;
@end



@implementation DetailViewController

@synthesize toolbar, popoverController, detailItem, detailDescriptionLabel, textField, tableDude;


-(IBAction)buttonPressed: (id)sender {
	NSString *curVal = textField.text;
	[curVal retain];
	
	NSString *curText = tableDude.text;
	tableDude.text = [[curText stringByAppendingString:@"\n"] stringByAppendingString:curVal];
	
	textField.text = [[NSString alloc] init];
	const char *str = [curVal UTF8String];
	
	(*cb_lispEval)(self, str);
	[curVal release];
}

#pragma mark -
#pragma mark Managing the detail item

/*
 When setting the detail item, update the view and dismiss the popover controller if it's showing.
 */
- (void)setDetailItem:(id)newDetailItem {
    if (detailItem != newDetailItem) {
        [detailItem release];
        detailItem = [newDetailItem retain];
        
        // Update the view.
        [self configureView];
    }

    if (self.popoverController != nil) {
        [self.popoverController dismissPopoverAnimated:YES];
    }        
}


- (void)configureView {
    // Update the user interface for the detail item.
    detailDescriptionLabel.text = [detailItem description];   
}


#pragma mark -
#pragma mark Split view support

- (void)splitViewController: (UISplitViewController*)svc willHideViewController:(UIViewController *)aViewController withBarButtonItem:(UIBarButtonItem*)barButtonItem forPopoverController: (UIPopoverController*)pc {
    
    barButtonItem.title = @"Root List";
    NSMutableArray *items = [[toolbar items] mutableCopy];
    [items insertObject:barButtonItem atIndex:0];
    [toolbar setItems:items animated:YES];
    [items release];
    self.popoverController = pc;
}


// Called when the view is shown again in the split view, invalidating the button and popover controller.
- (void)splitViewController: (UISplitViewController*)svc willShowViewController:(UIViewController *)aViewController invalidatingBarButtonItem:(UIBarButtonItem *)barButtonItem {
    
    NSMutableArray *items = [[toolbar items] mutableCopy];
    [items removeObjectAtIndex:0];
    [toolbar setItems:items animated:YES];
    [items release];
    self.popoverController = nil;
}


#pragma mark -
#pragma mark Rotation support

// Ensure that the view controller supports rotation and that the split view can therefore show in both portrait and landscape.
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation {
    return YES;
}


#pragma mark -
#pragma mark View lifecycle

// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad {
    [super viewDidLoad];
	self -> cb_lispEval = NULL;
	cb_viewDidLoad(self);
}

/*

 */

/*
- (void)viewWillAppear:(BOOL)animated {
    [super viewWillAppear:animated];
}
*/
/*
- (void)viewDidAppear:(BOOL)animated {
    [super viewDidAppear:animated];
}
*/
/*
- (void)viewWillDisappear:(BOOL)animated {
    [super viewWillDisappear:animated];
}
*/
/*
- (void)viewDidDisappear:(BOOL)animated {
    [super viewDidDisappear:animated];
}
*/

- (void)viewDidUnload {
    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
    self.popoverController = nil;
}


#pragma mark -
#pragma mark Memory management

/*
- (void)didReceiveMemoryWarning {
    // Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
    
    // Release any cached data, images, etc that aren't in use.
}
*/

- (void)dealloc {
    [popoverController release];
    [toolbar release];
    
    [detailItem release];
    [detailDescriptionLabel release];
    [super dealloc];
}

@end
