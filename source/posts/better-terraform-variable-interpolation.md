---
title: Better Terraform variable interpolation
date:  2018-05-05
---

If you use [Terraform](terraform), then you know that it’s configuration language HCL does not have the greatest facilities to deal with variables. This presents itself as soon as you want to use the contents of one variable on another. Here is an example of the problem many of you probably faced:

```
variable "ssh_key" {
  type        = "string"
  description = "filename for SSH key, without path"
}

resource "aws_key_pair" "foo" {
  key_name   = "${var.project_name}"
  public_key = "${file("${path.module}/${var.ssh_key}.pub")}"
}
```

So far so good. Except that as soon as you need to use contents for the private key, or refer to the path to either again, you have to repeat yourself. Worse yet, if you forget to change in one location but update in every other, you might be baffled when something stops working after what seems like a correct `terraform plan` and `terraform apply`. It seems like what we really want is something like the following:

```
variable "ssh_key" {
  type        = "map"
  description = "SSH map of maps with private and public keys for this project"
  default     = {
    private = {
      path = "${path.module}/id_ed25519"
      file = "${file("${var.ssh_key["private"]["path"]}")}"
    }
    public = {
      path = "${var.ssh_key["private"]["path"]}.pub"
      file = "${file("${var.ssh_key["public"]["path"]}")}"
    }
  }
}
```

Then you would access contents by `${var.ssh_key["private"]["file"]}`. There are (many) problems with this hypothetical approach, chief of which is that this is not valid Terraform HCL. The reasons are:

1. Variables cannot depend on the values of other variables
1. Maps of maps cannot be accessed with `"${var.foo["k₁"]["k₂"]}"`

The second restriction is easy, and can be resolved by using interpolation functions. Let’s investigate the first restriction. A few different solutions exist. There are pros and cons to each approach presented below, but they all amount to using either data sources or resources to emulate what `variable` could be doing. Judge with your team members what works best in your codebase(s).

## Null data sources

The easiest solution is to use a `null_data_source`, which does allow interpolation on its inputs based on previously created variables. It has [short documentation](null-data-source) available, which is more than enough. Our example from before would look like so using this:

```
variable "ssh_key_filename" {
  type        = "string"
  description = "name for the private SSH key on this project"
  default     = "id_ed25519"
}

data "null_data_source" "ssh_secret_key" {
  inputs = {
    path = "${path.module}/${var.ssh_key_filename}"
    file = "${file("${path.module}/${var.ssh_key_filename}")}"
  }
}

data "null_data_source" "ssh_public_key" {
  inputs = {
    path = "${data.null_data_source.ssh_secret_key.outputs["path"]}.pub"
    file = "${file("data.null_data_source.ssh_secret_key.outputs["path"]}.pub")}"
  }
}
```

This is almost there. Public key values are derived from private ones, and the filename itself is defined elsewhere. Of course, there is the duplication you see on `path` and `file` keys for both `null_data_source`s. If someone messes with either one of them you have the same problem as before. Can we make sure that everything is derived from other previous steps? Yes, but it looks (arguably) uglier:

```
variable "ssh_key_filename" {
  type        = "string"
  description = "name for the private SSH key on this project"
  default     = "id_ed25519"
}

data "null_data_source" "ssh_secret_key_path" {
  inputs = {
    value = "${path.module}/${var.ssh_key_filename}"
  }
}

data "null_data_source" "ssh_secret_key" {
  inputs = {
    value = "${file("${data.null_data_source.ssh_secret_key_path.outputs["value"]}")}"
  }
}

data "null_data_source" "ssh_public_key_path" {
  inputs = {
    value = "${data.null_data_source.ssh_secret_key_path.outputs["value"]}.pub"
  }
}

data "null_data_source" "ssh_public_key" {
  inputs = {
    value = "${file("${data.null_data_source.ssh_public_key_path.outputs["value"]}")}"
  }
}
```

If keys in `inputs` could refer to names previously defined within themselves, this would not be needed, but right now this is not the case. If you want to absolutely ensure all paths get updated when changing in a single location, the last version above is the best you can have right now with a `null_data_source`.

## Null resources

This is almost the same as the solution above, but the difference is that it implements the [resource lifecycle](https://www.terraform.io/docs/internals/lifecycle.html) instead of the one for data sources. You probably will not need this unless resources or modules that receive your `null_data_sources` are failing due to inexistent values before computing `count`. If you do receive this message, then read on.

We will use [null resources](https://www.terraform.io/docs/providers/null/resource.html). They track “triggers”, and when any of the triggers change values, the null resource itself is recreated. When this happens, any provisioners within it are run againg, using whatever connection you have defined. This is generally used to trigger sending files somewhere, or executing some code. We do not care about provisioners or connections, but we do care that triggers allow interpolation and expose themselves — something that is surprisingly undocumented. Any `inputs` from before will now be `triggers` parameters, and `outputs` are just exposed `triggers` attributes. Sounds strange, but an example makes it clear:

```
variable "ssh_key_filename" {
  type        = "string"
  description = "name for the private SSH key on this project"
  default     = "id_ed25519"
}

resource "null_resource" "ssh_secret_key_path" {
  triggers = {
    value = "${path.module}/${var.ssh_key_filename}"
  }
}

resource "null_resource" "ssh_secret_key" {
  triggers = {
    value = "${file("${null_data_source.ssh_secret_key_path.triggers["value"]}")}"
  }
}

resource "null_resource" "ssh_public_key_path" {
  triggers = {
    value = "${null_data_source.ssh_secret_key_path.triggers["value"]}.pub"
  }
}

resource "null_resource" "ssh_public_key" {
  triggers = {
    value = "${file("${null_data_source.ssh_public_key_path.triggers["value"]}")}"
  }
}
```

This is 100% guaranteed to calculate and expose the interpolated values before consuming resources use it as parameters, since the full resource lifecycle is implemented. The only disadvantage is that if you never seen this before, `terraform plan` with `null_resource` destruction and re-creations might be surprising.

## Template files

A final solution is to render a template using interpolation. This limits you to only having a single output consisting of the rendered template itself, but read on, as we can circumvent this. You also cannot use compound types such as lists or maps within `vars`. On the other hand, within the resource you have two levels of interpolation — at `vars` and on the template “file” itself. Our contrived example from before becomes now:

```
variable "ssh_key_filename" {
  type        = "string"
  description = "name for the private SSH key on this project"
  default     = "id_ed25519"
}

data "template_file" "ssh_secret_key" {
  template = "${file("$${path}")}"
  
  vars {
    path = "${path.module}/${var.ssh_key_filename}"
  }
}

data "template_file" "ssh_public_key" {
  template = "${file("$${path}")}"

  vars {
    path = "${data.template_file.ssh_secret_key.vars["path"]}.pub"
  }
}
```

This is much shorter, but it suffers from a few problems. First, it’s very easy to get confused with the escaping of variables using `$` or `$$`. Second, some values are retrieved using `data.template_file.foo.vars["name-of-var"]` while others come from  `data.template_file.foo.rendered`, and there is no easy way other than looking at source to know which is which. Lastly, `$` variables are interpolated _before_ any with `$$`, so you cannot have one nested within the other, and the only way to access `vars` from the template is using `$$`. It is also generally bad practice to use inline templates, as they disallow external checking, but in this case this is not applicable.

## Conclusion

None of the solutions above is perfect, but they have different trade-offs. As projects using Terraform grow and enforcing parameter correctness becomes apparent, you should consider which way you will handle your variables. My current guidelines for codebases I work on are:

1. If changing a value must immediately tell me if something’s wrong, define a `null_data_source`
1. If `count` is needed, convert `null_data_source` to `null_resource`
1. Document types and descriptions for either as comments
1. Provide documented variables for anything that must be user-configurable
1. Don’t use `template_file` unless you have an actual separate file that can be linted by itself 

Tooling at the Terraform registry for modules does not support showing documentation for any of these workarounds, but this is OK. As you can see from the examples, these interpolations are meant to be internal to the modules that use them. They substitute logic you would use a programming language to generate and enforce parameters at the Terraform DSL level, while foregoing many of the tests and checks that other languages would allow you to. Despite these restrictions, I still find it preferable to have logic be driven by code than by manual intervention, and until HCL supports variables referencing others, I will keep on using this. 

In fact, I think there is a certain advantage in that variables have this restriction. It disallows overriding what would be interpolated by invalid values that do not match what interpolation would reach. So in a cunkly way, this ends up being for the best. You just have to know when to use one form and another.

Code used in the examples above is available on [its own repository][variable-interpolation-repo]. If you notice that anything changed since this was published and what Terraform is capable, hit me up with an issue there so I can update this post and the code itself. Thank you!

[terraform]: https://www.terraform.io
[null-data-source]: https://www.terraform.io/docs/providers/null/data_source.html
[variable-interpolation-repo]: https://github.com/kerscher/terraform-examples-variable-interpolation
