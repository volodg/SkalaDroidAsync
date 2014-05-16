package org.jff.utils

import android.content.Context

object CurrentInterfaceLanguageCode {
  
  def currentInterfaceISO2LanguageCode(context: Context) : String = {

    context.getResources.getConfiguration.locale.getLanguage
  }
}