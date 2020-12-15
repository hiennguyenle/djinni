Pod::Spec.new do |s|
    s.name             = 'DjinniSwift'
    s.version          = '4.6.1'
    s.summary          = 'Djinni is a tool for generating cross-language type declarations and interface bindings.'
    s.description      = <<-DESC
    Djinni is a tool for generating cross-language type declarations and interface bindings.
    It's designed to connect C++ with either Java or Objective-C. Python support is experimental. 
    For more information see README.Python.md
                           DESC
    s.homepage         = 'https://github.com/hiennguyenle/finn'
    
    s.license          = { :type => 'MIT' }
    
    s.author           = { 'Hien Nguyen' => 'hien.nguyenle.it@gmail.com' }
    
    s.source           = { :git => 'https://github.com/hiennguyenle/finn.git', :tag => "#{s.version}" }
    
    s.module_name = 'DjinniSwift'
    
    s.ios.deployment_target  = '9.0'
    s.osx.deployment_target  = '10.10'
    
    s.source_files         = [
        "support-lib/swift/*.swift"
    ]
end
