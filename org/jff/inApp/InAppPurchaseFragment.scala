package org.jff.inApp

import org.jff.inApp.util._
import org.jff.utils.JLogTag
import org.jff.ui.{JActivityOnResult, JActivity}
import org.jff.inApp.util.IabHelper.OnIabSetupFinishedListener
import android.util.Log
import android.app.AlertDialog
import android.support.v4.app.Fragment
import android.content.{DialogInterface, Intent}
import com.wishdates.main.R

trait InAppPurchaseFragment extends Fragment  with JActivityOnResult {

  private var setupFinished: Boolean = false
  private var purchasingInProgress: Boolean = false
  private lazy val purchasedItems = collection.mutable.Set[String]()

  def consumePurchase(purchase: Purchase) {
    mHelper.consumeAsync(purchase, mConsumeFinishedListener)
  }

  def launchPurchaseFlow(productID: String) {

    if (purchasingInProgress)
      return

    Log.d(JLogTag.tag, "btn1Action")
    /* TODO: for security, generate your payload here for verification. See the comments on
     *        verifyDeveloperPayload() for more info. Since this is a SAMPLE, we just use
     *        an empty string, but on a production app you should carefully generate this. */
    val payload = ""

    if (setupFinished) {
      purchasingInProgress = true
      mHelper.launchPurchaseFlow(this.getActivity, productID, RC_REQUEST, mPurchaseFinishedListener, payload)
    } else {
      new AlertDialog.Builder(getActivity)
        .setTitle(R.string.NO_ACCOUNT_SET)
        .setMessage(R.string.OPEN_SETTINGS_QUESTION)
        .setPositiveButton(android.R.string.yes, new DialogInterface.OnClickListener() {
        def onClick(dialog: DialogInterface, which: Int) {
          startActivityForResult(new Intent(android.provider.Settings.ACTION_ADD_ACCOUNT), 0)
        }
      }).setNegativeButton(android.R.string.no, new DialogInterface.OnClickListener() {
        def onClick(dialog: DialogInterface, which: Int) {
          // do nothing
        }
      })
        .setIcon(android.R.drawable.ic_dialog_alert)
        .show()
    }
  }

  private val RC_REQUEST = 10001
  protected def base64EncodedPublicKey: String

  private var mHelper: IabHelper = null

  protected def initializeInAppPurchase() {

    deactivateInAppPurchase()
    val result = new IabHelper(getActivity, base64EncodedPublicKey)
    result.enableDebugLogging(true)
    result.startSetup(new OnIabSetupFinishedListener {
      def onIabSetupFinished(result: IabResult) = {

        if (mHelper != null && result.isSuccess) {
          // IAB is fully set up. Now, let's get an inventory of stuff we own.
          Log.d(JLogTag.tag, "Setup successful. Querying inventory.")
          mHelper.queryInventoryAsync(mGotInventoryListener)
          setupFinished = true
        }
      }
    })

    getActivity.asInstanceOf[JActivity].addOnActivityResultObserver(this)

    mHelper = result
  }

  def deactivateInAppPurchase() {

    getActivity.asInstanceOf[JActivity].removeOnActivityResultObserver(this)
    // very important:
    if (mHelper != null) {
      mHelper.dispose()
      mHelper = null
    }
  }

  protected def onSuccessLoadInventory(inventory: Inventory)

  // Listener that's called when we finish querying the items and subscriptions we own
  protected lazy val mGotInventoryListener = new IabHelper.QueryInventoryFinishedListener() {

    def onQueryInventoryFinished(result: IabResult, inventory: Inventory) {
      Log.d(JLogTag.tag, "Query inventory finished.")

      // Have we been disposed of in the meantime? If so, quit.
      if (mHelper != null && !result.isFailure) {

        Log.d(JLogTag.tag, "Query inventory was successful.")

        /*
         * Check for items we own. Notice that for each purchase, we check
         * the developer payload to see if it's correct! See
         * verifyDeveloperPayload().
        */

        // Check for gas delivery -- if we own gas, we should fill up the tank immediately
        inventory.getAllOwnedSkus.toArray.foreach(sku => {
          val purchase = inventory.getPurchase(sku.asInstanceOf[String])
          purchasedItems += purchase.getToken
        })
        onSuccessLoadInventory(inventory)
      }
    }
  }

  // Called when consumption is complete
  protected lazy val mConsumeFinishedListener = new IabHelper.OnConsumeFinishedListener() {
    def onConsumeFinished(purchase: Purchase, result: IabResult) {
      Log.d(JLogTag.tag, "Consumption finished. Purchase: " + purchase + ", result: " + result)
      // if we were disposed of in the meantime, quit.

      purchasingInProgress = false
      if (mHelper != null && result.isSuccess) {
        Log.d(JLogTag.tag, "Was consumed OK.")
      }
    }
  }

  protected def onSuccessPurchaseItem(purchase: Purchase)

  protected lazy val mPurchaseFinishedListener = new IabHelper.OnIabPurchaseFinishedListener() {
    def onIabPurchaseFinished(result: IabResult, purchase: Purchase) {
      Log.d(JLogTag.tag, "Purchase finished: " + result + ", purchase: " + purchase)

      // if we were disposed of in the meantime, quit.
      if (mHelper != null
        && !purchasedItems.contains(purchase.getToken)
        && result.isSuccess
        && verifyDeveloperPayload(purchase)) {

        Log.d(JLogTag.tag, "Purchase successful.")
        purchasedItems += purchase.getToken
        onSuccessPurchaseItem(purchase)
      }
    }
  }

  /** Verifies the developer payload of a purchase. */
  protected def verifyDeveloperPayload(p: Purchase): Boolean = {
    val payload = p.getDeveloperPayload

    /*
     * TODO: verify that the developer payload of the purchase is correct. It will be
     * the same one that you sent when initiating the purchase.
     *
     * WARNING: Locally generating a random string when starting a purchase and
     * verifying it here might seem like a good approach, but this will fail in the
     * case where the user purchases an item on one device and then uses your app on
     * a different device, because on the other device you will not have access to the
     * random string you originally generated.
     *
     * So a good developer payload has these characteristics:
     *
     * 1. If two different users purchase an item, the payload is different between them,
     *    so that one user's purchase can't be replayed to another user.
     *
     * 2. The payload must be such that you can verify it even when the app wasn't the
     *    one who initiated the purchase flow (so that items purchased by the user on
     *    one device work on other devices owned by the user).
     *
     * Using your own server to store and verify developer payloads across app
     * installations is recommended.
     */

    true
  }

  override def onJActivityResult(requestCode: Int, resultCode: Int, data: Intent): Boolean = {
    Log.d(JLogTag.tag, "onActivityResult(" + requestCode + "," + resultCode + "," + data)
    if (mHelper != null) {

      // Pass on the activity result to the helper for handling
      if (mHelper.handleActivityResult(requestCode, resultCode, data)) {
        Log.d(JLogTag.tag, "onActivityResult handled by IABUtil.")
        true
      }
      else {
        // not handled, so handle it ourselves (here's where you'd
        // perform any handling of activity results not related to in-app
        // billing...
        false
      }
    }
    false
  }

  override def onDestroy() {
    super.onDestroy()
    deactivateInAppPurchase()
  }
}
