package com.apex

import supertagged.TaggedType

package object common {

  object Value extends TaggedType[Long]
  object Nonce extends TaggedType[Long]

  type Value = Value.Type
  type Nonce = Nonce.Type
}
