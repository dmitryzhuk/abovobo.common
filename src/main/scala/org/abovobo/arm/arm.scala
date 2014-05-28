package org.abovobo.arm

package object arm {
	def using[T <: { def close() }]
	    (resource: T)
	    (block: T => Unit) 
	{
	  try {
	    block(resource)
	  } finally {
	    if (resource != null) resource.close()
	  }
	}
	
	def using[T <: { def close() }, U <: { def close() }]
	    (resource1: T, resource2: U)
	    (block: T => U => Unit) 
	{
	  using(resource1) { r1 =>
	    using(resource2) { r2 =>
	      block(r1)(r2)
	    }
	  }
	}
}