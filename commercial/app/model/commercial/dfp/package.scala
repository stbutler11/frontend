package model.commercial

import com.google.api.ads.common.lib.auth.OfflineCredentials
import com.google.api.ads.common.lib.auth.OfflineCredentials.Api
import com.google.api.ads.dfp.lib.client.DfpSession
import com.google.api.ads.dfp.axis.factory.DfpServices

package object dfp {

  val networkId = "59739193"

  val orderId = 156599313

  val sectionKeyId = 363393L
  val keywordKeyId = 363513L

  val oAuth2Credential = new OfflineCredentials.Builder()
    .forApi(Api.DFP)
    .fromFile()
    .build()
    .generateCredential()

  val session = new DfpSession.Builder()
    .fromFile()
    .withOAuth2Credential(oAuth2Credential)
    .build()
  session.setNetworkCode(networkId)

  val dfpServices = new DfpServices()
}